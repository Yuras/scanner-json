
module Main
( main
)
where

import qualified Scanner
import qualified Scanner.JSON as Scanner (json)
import qualified Data.ByteString as ByteString
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Aeson.Parser as Aeson
import Control.Monad
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("aeson" : f : _) -> aeson f
    ("scanner": f :_) -> scanner f
    _ -> error "args: aeson/scanner file.json"

scanner :: FilePath -> IO ()
scanner path = withBinaryFile path ReadMode $ \h -> do
  input <- more h
  loop h input
  where
  loop h input = do
    res <- Scanner.scanWith (more h) Scanner.json input
    case res of
      Scanner.Done rest _r -> do
        --print _r
        if ByteString.null rest
          then do
            input' <- more h
            unless (ByteString.null input') $
              loop h input'
          else loop h rest
      Scanner.Fail _ err -> fail err
      Scanner.More _ -> error "more?"
  more h = ByteString.hGetSome h (32 * 1000)

aeson :: FilePath -> IO ()
aeson path = withBinaryFile path ReadMode $ \h -> do
  input <- more h
  loop h input
  where
  loop h input = do
    res <- Atto.parseWith (more h) Aeson.json input
    case res of
      Atto.Done rest _r -> do
        --print _r
        if ByteString.null rest
          then do
            input' <- more h
            unless (ByteString.null input') $
              loop h input'
          else loop h rest
      Atto.Fail _ _ err -> fail err
      Atto.Partial _ -> error "more?"
  more h = ByteString.hGetSome h (32 * 1000)
