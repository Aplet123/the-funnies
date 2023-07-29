import { constants } from "node:os";

const internal: any = {};
(process as any).dlopen({ exports: internal }, __dirname + "/internal.node", (constants as any).dlopen.RTLD_NOW | (constants as any).dlopen.RTLD_GLOBAL);

export class PyVal extends Function {
    static shouldProxy: boolean = true;

    private inner: unknown;
    unproxied: PyVal;

    constructor(inner: unknown) {
        super();
        this.inner = inner;
        this.unproxied = this;
    }

    toString(): string {
        return internal.py_str(this.inner);
    }

    repr(): string {
        return internal.py_repr(this.inner);
    }

    [Symbol.for("nodejs.util.inspect.custom")](_depth: number, _opts: object, _inspect: Function): string {
        return this.repr();
    }

    getattr(attr: string): PyVal {
        return new PyVal(internal.py_getattr(this.inner, attr)).proxied(true);
    }

    hasattr(attr: string): boolean {
        return internal.py_hasattr(this.inner, attr);
    }

    call(...args: unknown[]): PyVal {
        let kwargs = new Map();
        if (args.length > 0) {
            const last = args[args.length - 1];
            // direct non-subclassed object
            if (typeof last === "object" && last !== null && last.constructor === Object) {
                kwargs = new Map(Object.entries(last).map(([k, v]) => [k, v instanceof PyVal ? v.inner : v]));
                args.pop();
            }
        }
        const argList = [];
        for (const v of args) {
            argList.push(v instanceof PyVal ? v.inner : v);
        }
        return new PyVal(
            internal.py_call(this.inner, internal.py_tuple(args.map((v) => (v instanceof PyVal ? v.inner : v))), internal.js_to_py(kwargs))
        ).proxied(true);
    }

    toJs(): unknown {
        return internal.py_to_js(this.inner);
    }

    proxied(useSetting: boolean = false): PyVal {
        if (useSetting && !PyVal.shouldProxy) {
            return this;
        }
        return new Proxy(this, {
            get(target, p, receiver): unknown {
                if (typeof p === "symbol" || p in target) {
                    const ret = (target as any)[p];
                    return ret;
                }
                return target.getattr(p);
            },
            has(target, p): boolean {
                if (p in target) {
                    return true;
                } else if (typeof p === "symbol") {
                    return false;
                }
                return target.hasattr(p);
            },
            apply(target, thisArg, args): unknown {
                return target.call(...args);
            },
        });
    }

    static fromJs(val: unknown): PyVal {
        return new PyVal(internal.js_to_py(val)).proxied(true);
    }

    static imp(mod: string): PyVal {
        return new PyVal(internal.py_import(mod)).proxied(true);
    }

    static eval(expr: string): PyVal {
        return new PyVal(internal.py_eval(expr)).proxied(true);
    }

    static exec(code: string): PyVal {
        return new PyVal(internal.py_exec(code)).proxied(true);
    }

    static tuple(...vals: unknown[]): PyVal {
        return new PyVal(internal.py_tuple(vals)).proxied(true);
    }
}
