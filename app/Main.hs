{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import qualified Data.Text                 as T
import           MathLanguage
import           MathParser
import           System.Environment        (getArgs)

readArg :: MaybeT IO T.Text
readArg = do
  args <- lift getArgs
  guard (not $ null args)
  return . T.pack . head $ args

main :: IO ()
main = do
  res <- runMaybeT $ do parseMathExpression <$> readArg
  case res of
    Nothing -> putStrLn "Oh no"
    Just r ->
      case r of
        Left e   -> print e
        Right re -> print . eval $ re
