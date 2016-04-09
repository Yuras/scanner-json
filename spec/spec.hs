{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Scanner.JSON

import qualified Scanner
import Data.Either
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "json" $ do
    it "should parse string" $ do
      let input = "\"hello\""
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.String "hello")

    it "should parse escaped string" $ do
      let input = "\"he\\nllo\""
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.String "he\nllo")

    it "should parse hex escaped string" $ do
      let input = "\"he\\u0416llo\""
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.String "heЖllo")

    it "should parse null" $ do
      let input = "null"
          v = Scanner.scanOnly json input
      v `shouldBe` Right Aeson.Null

    it "should parse true" $ do
      let input = "true"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Bool True)

    it "should parse false" $ do
      let input = "false"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Bool False)

    it "should parse integer" $ do
      let input = "123"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 123)

    it "should parse negative integer" $ do
      let input = "-123"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number (-123))

    it "should parse float" $ do
      let input = "123.45"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 123.45)

    it "should parse integer with exponent" $ do
      let input = "123e2"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 12300)

    it "should parse integer with positive exponent" $ do
      let input = "123e+2"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 12300)

    it "should parse integer with negative exponent" $ do
      let input = "123e-2"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 1.23)

    it "should parse float with exponent" $ do
      let input = "123.45e1"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 1234.5)

    it "should parse float with positive exponent" $ do
      let input = "123.45e+1"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 1234.5)

    it "should parse float with negative exponent" $ do
      let input = "123.45e-1"
          v = Scanner.scanOnly json input
      v `shouldBe` Right (Aeson.Number 12.345)

    it "should fail on misspelled keyword" $ do
      let input = "hull"
          v = Scanner.scanOnly json input
      v `shouldSatisfy` isLeft

    it "should parse array" $ do
      let input = "[true,false]"
          v = Scanner.scanOnly json input
          res = Aeson.Array $ Vector.fromList
            [ Aeson.Bool True
            , Aeson.Bool False
            ]
      v `shouldBe` Right res

    it "should parse empty array" $ do
      let input = "[]"
          v = Scanner.scanOnly json input
          res = Aeson.Array Vector.empty
      v `shouldBe` Right res

    it "should parse array with spaces" $ do
      let input = "[ true , false ]"
          v = Scanner.scanOnly json input
          res = Aeson.Array $ Vector.fromList
            [ Aeson.Bool True
            , Aeson.Bool False
            ]
      v `shouldBe` Right res

    it "should fail on missing ',' in array" $ do
      let input = "[true false]"
          v = Scanner.scanOnly json input
      v `shouldSatisfy` isLeft

    it "should parse dictionary" $ do
      let input = "{\"a\":true,\"b\":false}"
          v = Scanner.scanOnly json input
          res = Aeson.object
            [ ("a", Aeson.Bool True)
            , ("b", Aeson.Bool False)
            ]
      v `shouldBe` Right res

    it "should parse empty dictionary" $ do
      let input = "{}"
          v = Scanner.scanOnly json input
          res = Aeson.object []
      v `shouldBe` Right res

    it "should parse dictionary with spaces" $ do
      let input = "{ \"a\" : true , \"b\" : false }"
          v = Scanner.scanOnly json input
          res = Aeson.object
            [ ("a", Aeson.Bool True)
            , ("b", Aeson.Bool False)
            ]
      v `shouldBe` Right res

    it "should fail on missing ':' in dictionary" $ do
      let input = "{\"a\":true \"b\":false}"
          v = Scanner.scanOnly json input
      v `shouldSatisfy` isLeft

    it "should fail on missing '\"' in dictionary key" $ do
      let input = "{a\":true,\"b\":false}"
          v = Scanner.scanOnly json input
      v `shouldSatisfy` isLeft

    it "should fail on trailing ',' in dictionary" $ do
      let input = "{\"a\":true,\"b\":false,}"
          v = Scanner.scanOnly json input
      v `shouldSatisfy` isLeft
