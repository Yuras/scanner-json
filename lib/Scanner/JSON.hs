{-# LANGUAGE OverloadedStrings #-}

module Scanner.JSON
( json
)
where

import Scanner (Scanner)
import qualified Scanner

import Prelude hiding (null, exponent)
import Data.Bits
import Data.Char (chr, digitToInt)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString (unsafeIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Control.Monad

{-# INLINE json #-}
json :: Scanner Value
json = do
  skipSpace
  c <- Scanner.anyChar8
  json_ c

{-# INLINE skipSpace #-}
skipSpace :: Scanner ()
skipSpace = Scanner.skipSpace

{-# INLINE json_ #-}
json_ :: Char -> Scanner Value
json_ c =
  case c of
    '"' -> string
    '[' -> array
    '{' -> dict
    'n' -> null
    't' -> true
    'f' -> false
    '-' -> do
      n <- number 0
      return (Aeson.Number (negate n))
    _ -> if c >= '0' && c <= '9'
          then Aeson.Number <$> number (fromIntegral $ digitToInt c)
          else fail "expected value"

{-# INLINE number #-}
number :: Integer -> Scanner Scientific
number i = do
  int <- decimal i
  c <- Scanner.lookAheadChar8
  if c == Just '.'
    then do
      void $ Scanner.anyChar8
      real int
    else exponent int 0
  where
  exponent int ex = do
    c <- Scanner.lookAheadChar8
    if c == Just 'e' || c == Just 'E'
      then do
        void $ Scanner.anyChar8
        c' <- Scanner.anyChar8
        case c' of
          '-' -> do
            e <- negate <$> decimal 0
            return (Scientific.scientific int (e + ex))
          '+' -> do
            e <- decimal 0
            return (Scientific.scientific int (e + ex))
          _ -> do
            e <- decimal (fromIntegral $ digitToInt c')
            return (Scientific.scientific int (e + ex))
      else return (Scientific.scientific int ex)

  real int = do
    bs <- Scanner.takeWhile (\w -> w - 48 <= 9)
    let int' = ByteString.foldl' step int bs
        step a w = a * 10 + fromIntegral (w - 48)
    exponent int' (negate $ ByteString.length bs)

decimal :: Integral i => i -> Scanner i
decimal i = do
  bs <- Scanner.takeWhile (\w -> w - 48 <= 9)
  return $ ByteString.foldl' step i bs
  where
  step a w = a * 10 + fromIntegral (w - 48)

{-# INLINE string #-}
string :: Scanner Value
string = do
  txt <- text
  return (Aeson.String txt)

{-# INLINE text #-}
text :: Scanner Text
text = do
  str <- Scanner.takeWhileChar8 (\c -> c /= '"' && c /= '\\')
  c <- Scanner.anyChar8
  case c of
    '"' -> return (Text.decodeUtf8 str)
    '\\' -> go [Text.decodeUtf8 str]
    _ -> error "Scanner.JSON.text: impossible"
  where
  go res = do
    c <- Scanner.anyChar8
    case c of
      '"' -> go' ("\"" : res)
      '\\' -> go' ("\\" : res)
      '/' -> go' ("/" : res)
      'b' -> go' ("\b" : res)
      'f' -> go' ("\f" : res)
      'n' -> go' ("\n" : res)
      'r' -> go' ("\r" : res)
      't' -> go' ("\t" : res)
      'u' -> do
        u <- hexUnescape
        go' (u : res)
      _ -> fail "unknown escape sequence"

  go' res = do
    str <- Scanner.takeWhileChar8 (\c -> c /= '"' && c /= '\\')
    c <- Scanner.anyChar8
    case c of
      '"' -> return (Text.concat $ reverse $ Text.decodeUtf8 str : res)
      '\\' -> go (Text.decodeUtf8 str : res)
      _ -> error "Scanner.JSON.text: impossible"

  hexUnescape = do
    h <- hex4
    return (Text.pack [chr h])
  hex4 = do
    bs <- Scanner.take 4
    let unhex n | w >= 48 && w <= 57 = w - 48
                | w >= 97 && w <= 102 = w - 87
                | w >= 65 && w <= 70 = w - 55
                | otherwise = 255
          where
          w = fromIntegral $ ByteString.unsafeIndex bs n
        a = unhex 0
        b = unhex 1
        c = unhex 2
        d = unhex 3
    when ((a .|. b .|. c .|. d) == 255) $
      fail "invalid hex escape"

    return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)

{-# INLINE null #-}
null :: Scanner Value
null = do
  Scanner.string "ull"
  return Aeson.Null

{-# INLINE true #-}
true :: Scanner Value
true = do
  Scanner.string "rue"
  return (Aeson.Bool True)

{-# INLINE false #-}
false :: Scanner Value
false = do
  Scanner.string "alse"
  return (Aeson.Bool False)

{-# NOINLINE array #-}
array :: Scanner Value
array = go []
  where
  go res = do
    skipSpace
    c <- Scanner.anyChar8
    case c of
      ']' -> return (Aeson.Array $ Vector.reverse (Vector.fromList res))
      _ -> do
        v <- json_ c
        go' (v : res)

  go' res = do
    skipSpace
    c <- Scanner.anyChar8
    case c of
      ']' -> return (Aeson.Array $ Vector.reverse (Vector.fromList res))
      ',' -> go res
      _ -> fail "expected ',' or ']'"

{-# NOINLINE dict #-}
dict :: Scanner Value
dict = do
  skipSpace
  c <- Scanner.anyChar8
  case c of
    '}' -> return (Aeson.Object HashMap.empty)
    '"' -> do
      p <- pair
      go [p]
    _ -> fail "expected '\"' or '}'"
  where
  pair = do
    k <- text
    skipSpace
    Scanner.char8 ':'
    skipSpace
    v <- json
    return (k, v)

  go res = do
    skipSpace
    c <- Scanner.anyChar8
    case c of
      '}' -> return (Aeson.object res)
      ',' -> do
        skipSpace
        Scanner.char8 '"'
        p <- pair
        go (p:res)
      _ -> fail "expected ',' or '}'"

{-
dict_ :: Scanner Value
dict_ = do
  skipSpace
  c <- Scanner.anyChar8
  case c of
    '}' -> return (Aeson.Object HashMap.empty)
    '"' -> do
      (k, v) <- pair
      go (HashMap.singleton k v)
    _ -> fail "expected '\"' or '}'"
  where
  pair = do
    k <- text
    skipSpace
    Scanner.char8 ':'
    skipSpace
    v <- json
    return (k, v)

  go res = do
    skipSpace
    c <- Scanner.anyChar8
    case c of
      '}' -> return (Aeson.Object res)
      ',' -> do
        skipSpace
        Scanner.char8 '"'
        (k, v) <- pair
        go $! (HashMap.insert k v res)
      _ -> fail "expected ',' or '}'"
      -}
