{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified Scanner
import qualified Scanner.JSON as Scanner (json)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as Atto
import Data.Aeson (Value)
import qualified Data.Aeson.Parser as Aeson (json)

import Criterion
import Criterion.Main

main :: IO ()
main = do
  let json = "{\"hello\":\"wor\\nld\", \"a\":{\"b\":true}, \"c\": [123, -123.43e2, null]}"
  print json
  print $ jsonScanner json
  print $ jsonAeson json
  defaultMain
    [ bgroup "json"
      [ bench "scanner" $ whnf jsonScanner json
      , bench "aeson" $ whnf jsonAeson json
      ]
    ]

{-# NOINLINE jsonScanner #-}
jsonScanner :: ByteString -> Either String Value
jsonScanner = Scanner.scanOnly Scanner.json

{-# NOINLINE jsonAeson #-}
jsonAeson :: ByteString -> Either String Value
jsonAeson = Atto.parseOnly Aeson.json
