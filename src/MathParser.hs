{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module MathParser (parseMathExpression) where

import MathLanguage
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- basic lexer setup
spaceConsumer :: Parser ()
-- there are no comments in math expressions, so just pass `empty` for both
-- comment parsers
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

constantP :: Parser Expression
constantP = Constant <$> (try (lexeme L.float)
    <|> ((fromIntegral :: Int -> Double) <$> lexeme L.decimal))

parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

termP :: Parser Expression
termP = (parenthesized expressionP) <|> constantP

expressionP :: Parser Expression
expressionP = makeExprParser termP operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable = [[prefix "-" Neg, prefix "+" id],
                 [binary "*" Mul, binary "/" Div],
                 [binary "+" Add, binary "-" Sub]]
    where
        prefix :: T.Text -> (Expression -> Expression) 
               -> Operator Parser Expression
        prefix name fun = Prefix (fun <$ symbol name)

        binary :: T.Text -> (Expression -> Expression -> Expression) 
               -> Operator Parser Expression
        binary name fun = InfixL (fun <$ symbol name)

parseMathExpression :: T.Text
                    -> Either (ParseErrorBundle T.Text Void) Expression
parseMathExpression = parse expressionP ""
