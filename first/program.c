#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>

const char stuff[] = "%d\n\0Fizz\0Buzz\0Rizz\0Jazz\0Dizz\0Prizz";

char* mask(char* ptr) {
    return (char*)((size_t)ptr & ~0xfffULL);
}

void emit_print(char** ptr, int offset) {
    char data[9] = {0x48, 0x8d, 0xbb, 0, 0, 0, 0, 0xff, 0xd5};
    // this is UB but guess what so is half this program
    *(int*)(data + 3) = offset;
    memcpy(*ptr, data, sizeof(data));
    *ptr += sizeof(data);
}

void emit_print_num(char** ptr, size_t num) {
    char data[15] = {0x48, 0x89, 0xdf, 0x48, 0xbe, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xd5};
    *(size_t*)(data + 5) = num;
    memcpy(*ptr, data, sizeof(data));
    *ptr += sizeof(data);
}

void emit_jump(char** ptr, char* target) {
    char data[5] = {0xe9, 0, 0, 0, 0};
    *(int*)(data + 1) = target - (*ptr + 5);
    memcpy(*ptr, data, sizeof(data));
    *ptr += sizeof(data);
}

int main(void) {
    const int bound = 100;
    char** ptrs = malloc(bound * sizeof(char*));
    // ternary operators are not if statements suck it up
    for (int i = 0; i < bound; i ++) {
        ptrs[i] = mmap(i == 0 ? NULL : ptrs[i - 1] + 0x1000, 0x1000, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
    }
    char* ptr = mmap(NULL, 0x10000, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
    void (*orig)() = (void(*)()) ptr;
    // the 0x50 is just there for stack alignment don't bully me
    char header[23] = {0x55, 0x53, 0x50, 0x48, 0xbb, 0, 0, 0, 0, 0, 0, 0, 0, 0x48, 0xbd, 0, 0, 0, 0, 0, 0, 0, 0};
    *(size_t*)(header + 5) = (size_t) &stuff;
    *(size_t*)(header + 15) = (size_t) &printf;
    memcpy(ptrs[0], header, sizeof(header));
    ptrs[0] += sizeof(header);
    // multiples of 3
    for (int i = 2; i < bound; i += 3) {
        emit_print(&ptrs[i], 4);
    }
    // multiples of 5
    for (int i = 4; i < bound; i += 5) {
        emit_print(&ptrs[i], 9);
    }
    // multiples of 7
    for (int i = 6; i < bound; i += 7) {
        emit_print(&ptrs[i], 14);
    }
    // multiples of 11
    for (int i = 10; i < bound; i += 11) {
        emit_print(&ptrs[i], 19);
    }
    // divisors of 120
    for (int i = 0; i < bound && i <= 120; i ++) {
        120 % (i + 1) == 0 ? emit_print(&ptrs[i], 24) : 0;
    }
    // primes
    for (int i = 1; i < bound; i ++) {
        *ptrs[i] = 0x90;
    }
    for (int i = 1; i < bound; i ++) {
        for (int j = i * 2 + 1; j < bound && (unsigned char) *ptrs[i] == 0x90; j += i + 1) {
            *ptrs[j] = 0;
        }
    }
    for (int i = 0; i < bound; i ++) {
        ((unsigned char) *ptrs[i] == 0x90) ? emit_print(&ptrs[i], 29) : 0;
    }
    // print either newline or number
    for (int i = 0; i < bound; i ++) {
        ptrs[i] == mask(ptrs[i]) ? emit_print_num(&ptrs[i], i + 1) : emit_print(&ptrs[i], 2);
    }
    // add jumps from one segment to the next
    for (int i = 0; i < bound - 1; i ++) {
        emit_jump(&ptrs[i], mask(ptrs[i + 1]));
    }
    char footer[4] = {0x58, 0x5b, 0x5d, 0xc3};
    memcpy(ptrs[bound - 1], footer, sizeof(footer));
    ptrs[bound - 1] += sizeof(footer);
    ((void(*)())mask(ptrs[0]))();
    free(ptrs);
    return 0;
}
