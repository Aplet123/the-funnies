{-# LANGUAGE OverloadedStrings #-}

import Codec.Compression.Zlib.Raw
import Control.Monad.Writer.Lazy
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Data.Word
import System.Environment
import System.IO
import System.Posix.Time
import Text.Read

type SectionWriter = Writer Builder

adler32 :: ByteString -> Word32
adler32 s = (b `shiftL` 16) .|. a
  where
    f x = x `mod` 65521
    (a, b) = foldl' (\(a, b) x -> let a' = f $ a + fromIntegral x in (a', f $ b + a')) (1, 0) (B.unpack s)

crc32 :: ByteString -> Word32
crc32 =
  complement
    . foldl'
      (\crc x -> iterate (\n -> (n `shiftR` 1) `xor` (0xedb88320 .&. (-(n .&. 1)))) (crc `xor` fromIntegral x) !! 8)
      0xffffffff
    . B.unpack

packBE :: (Integral a, FiniteBits a) => a -> ByteString
packBE n = B.pack . fst $ foldl' (\(l, x) _ -> (fromIntegral (x .&. 0xff) : l, x `shiftR` 8)) ([], n) [1 .. finiteBitSize n `div` 8]

packLE :: (Integral a, FiniteBits a) => a -> ByteString
packLE = B.reverse . packBE

unpackBE :: (Integral a) => ByteString -> a
unpackBE = foldl' (\a b -> a * 0x100 + fromIntegral b) 0 . B.unpack

unpackLE :: (Integral a) => ByteString -> a
unpackLE = unpackBE . B.reverse

writeBytes :: ByteString -> SectionWriter ()
writeBytes h = writer ((), BB.lazyByteString h)

writeBE32 :: Word32 -> SectionWriter ()
writeBE32 = writeBytes . packBE

writeByte :: Word8 -> SectionWriter ()
writeByte = writeBytes . B.pack . singleton

finishSection :: SectionWriter a -> ByteString
finishSection w = packBE (fromIntegral (B.length b - 4) :: Word32) <> b <> packBE (crc32 b)
  where
    b = BB.toLazyByteString $ execWriter w

makePngHeader :: ByteString
makePngHeader = B.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

makeIhdr :: Word32 -> Word32 -> ByteString
makeIhdr width height = finishSection $ do
  writeBytes "IHDR"
  writeBE32 width
  writeBE32 height
  writeBytes $ B.pack [8, 6, 0, 0, 0]

makeIend :: ByteString
makeIend = finishSection $ writeBytes "IEND"

dims :: Array (Int, Int, Int) a -> (Int, Int, Int)
dims a = let ((x, y, z), (x', y', z')) = bounds a in (x' - x + 1, y' - y + 1, z' - z + 1)

writeImageData :: Int -> Array (Int, Int, Int) Word32 -> SectionWriter ()
writeImageData t a = do
  let ((_, y, z), (_, y', z')) = bounds a
  writeByte 0x08
  writeByte 0xd7
  let dat = B.concat $ map (\r -> B.cons 0x00 . B.concat . map (\c -> packLE . complement $ a ! (t, r, c)) $ [z .. z']) [y .. y']
  writeBytes $ compress dat
  writeBE32 $ adler32 dat

makeIdat :: Int -> Array (Int, Int, Int) Word32 -> ByteString
makeIdat t a = finishSection $ writeBytes "IDAT" >> writeImageData t a

makeFdat :: Int -> Array (Int, Int, Int) Word32 -> ByteString
makeFdat t a = finishSection $ writeBytes "fdAT" >> writeBE32 (fromIntegral $ t * 2) >> writeImageData t a

makeText :: ByteString -> ByteString -> ByteString
makeText k v = finishSection $ do
  writeBytes "tEXt"
  writeBytes k
  writeByte 0x00
  writeBytes v

makeActl :: Int -> ByteString
makeActl t = finishSection $ do
  writeBytes "acTL"
  writeBE32 $ fromIntegral t
  writeBE32 0

makeFctl :: Int -> Int -> Int -> ByteString
makeFctl w h t = finishSection $ do
  writeBytes "fcTL"
  writeBE32 . fromIntegral $ if t == 0 then 0 else t * 2 - 1
  writeBE32 $ fromIntegral w
  writeBE32 $ fromIntegral h
  writeBE32 0
  writeBE32 0
  writeBytes $ packBE (1 :: Word16)
  writeBytes $ packBE (10 :: Word16)
  writeByte 0
  writeByte 0

makeImage :: String -> String -> Array (Int, Int, Int) Word32 -> ByteString
makeImage author time a =
  fold
    ( [ makePngHeader,
        makeIhdr (fromIntegral w) (fromIntegral h),
        makeActl d
      ]
        ++ map
          ( \t ->
              makeFctl w h t
                <> (if t == 0 then makeIdat else makeFdat) t a
          )
          [0 .. d - 1]
        ++ [ makeText "Author" (C.pack author),
             makeText "Creation Date" (C.pack time),
             makeIend
           ]
    )
  where
    (d, h, w) = dims a

testData :: Array (Int, Int, Int) Word32
testData =
  array
    ((0, 0, 0), (10, 10, 10))
    [ ( (i, j, k),
        0xff `shiftL` 24
          .|. fromIntegral (i * 20) `shiftL` 16
          .|. fromIntegral (j * 20) `shiftL` 8
          .|. fromIntegral (k * 20) `shiftL` 0
      )
      | i <- [0 .. 10],
        j <- [0 .. 10],
        k <- [0 .. 10]
    ]

listTo3dArray :: [[[a]]] -> Maybe (Array (Int, Int, Int) a)
listTo3dArray l = do
  h <- listToMaybe l
  hh <- listToMaybe h
  let x = length l
  let y = length h
  let z = length hh
  return $
    array
      ((0, 0, 0), (x - 1, y - 1, z - 1))
      [((i, j, k), v) | (i, l') <- zip [0 ..] l, (j, l'') <- zip [0 ..] l', (k, v) <- zip [0 ..] l'']

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

dump :: IO ()
dump = do
  time <- epochTime
  author <- prompt "Author? "
  outfile <- prompt "Outfile? "
  dat <- prompt "Data?\n" <&> (readMaybe >=> listTo3dArray)
  case dat of
    Just d -> B.writeFile outfile $ makeImage author (show time) d
    Nothing -> hPutStrLn stderr "Invalid input data."

getSections :: ByteString -> Maybe [(ByteString, ByteString)]
getSections dat = do
  guard $ B.take 8 dat == makePngHeader
  return $
    unfoldr
      ( \b ->
          if B.null b
            then Nothing
            else
              let len = unpackBE $ B.take 4 b
               in Just ((B.take 4 . B.drop 4 $ b, B.take len . B.drop 8 $ b), B.drop (len + 12) b)
      )
      (B.drop 8 dat)

splitFirstNull :: ByteString -> (ByteString, ByteString)
splitFirstNull b = let (k, v) = B.span (/= 0) b in (k, B.drop 1 v)

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f (x : xs) =
  case f x of
    Just v -> Just v
    Nothing -> findMap f xs
findMap f _ = Nothing

getMetadata :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
getMetadata k =
  findMap
    ( \(s, e) ->
        let (k', v) = splitFirstNull e
         in if s == "tEXt" && k == k' then Just v else Nothing
    )

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n l = let (a, b) = splitAt n l in a:chunks n b

getWidth :: [(ByteString, ByteString)] -> Maybe Int
getWidth = fmap (unpackBE . B.take 4) . lookup "IHDR"

dropEnd :: Int -> BS.ByteString -> BS.ByteString
dropEnd n b = BS.take (BS.length b - n) b

getImageData :: Int -> [(ByteString, ByteString)] -> [[[Word32]]]
getImageData width =
  mapMaybe (\(k, e) ->
    case k of
      "IDAT" -> Just $ f (B.fromStrict . BS.drop 2 . dropEnd 4 . B.toStrict $ e)
      "fdAT" -> Just $ f (B.fromStrict . BS.drop 6 . dropEnd 4 . B.toStrict $ e)
      _ -> Nothing)
  where f = map (map (complement . unpackLE . B.pack) . chunks 4 . tail) . chunks (width * 4 + 1) . B.unpack . decompress

fullParse :: [(ByteString, ByteString)] -> Maybe (String, String, [[[Word32]]])
fullParse s = do
  width <- getWidth s
  author <- getMetadata "Author" s
  timestamp <- getMetadata "Creation Date" s
  let dat = getImageData width s
  return (C.unpack author, C.unpack timestamp, dat)

load :: IO ()
load = do
  filename <- prompt "Infile? "
  content <- B.readFile filename
  case getSections content >>= fullParse of
    Just (author, timestamp, dat) ->
      putStrLn $ concat ["Author: ", author, "\nCreation date: ", timestamp, "\nData:\n", show dat]
    Nothing -> hPutStrLn stderr "Input file is invalid."

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just "dump" -> dump
    Just "load" -> load
    _ -> hPutStrLn stderr "Please run with 'dump' or 'load' as the first argument."