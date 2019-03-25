{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text       (Text)
import           Data.Void
import           MathLanguage
import           MathParser
import           Test.Hspec
import           Text.Megaparsec (ParseErrorBundle)

parseAndEval :: Text -> Either (ParseErrorBundle Text Void) Double
parseAndEval text = fmap eval (parseMathExpression text)

main :: IO ()
main =
  hspec $ do
    describe "MathParser" $ do
      it "can parse simple expressions in one operator" $ do
        parseMathExpression "1 + 1" `shouldBe`
          Right (Add (Constant 1) (Constant 1))
        parseMathExpression "1 - 1" `shouldBe`
          Right (Sub (Constant 1) (Constant 1))
        parseMathExpression "1 * 1" `shouldBe`
          Right (Mul (Constant 1) (Constant 1))
        parseMathExpression "1 / 1" `shouldBe`
          Right (Div (Constant 1) (Constant 1))
      it "can parse prefix expressions" $ do
        parseMathExpression "-1" `shouldBe` Right (Neg (Constant 1))
        parseMathExpression "+1" `shouldBe` Right (Constant 1)
        parseMathExpression "-(1 + 1)" `shouldBe`
          Right (Neg (Add (Constant 1) (Constant 1)))
        parseMathExpression "+(1 * 1)" `shouldBe`
          Right (Mul (Constant 1) (Constant 1))
      it "can parse expressions with correct precedence" $ do
        parseMathExpression "1 + 2 * 3 + 4" `shouldBe`
          Right
            (Add (Add (Constant 1) (Mul (Constant 2) (Constant 3))) (Constant 4))
        parseMathExpression "-3 * 4" `shouldBe`
          Right (Mul (Neg (Constant 3)) (Constant 4))
    describe "Evaluation" $ do
      it "works for simple expressions in one operator" $ do
        parseAndEval "1 + 2" `shouldBe` Right 3
        parseAndEval "2 - 8" `shouldBe` Right (-6)
        parseAndEval "3 * 5" `shouldBe` Right 15
        parseAndEval "4 / 2" `shouldBe` Right 2
