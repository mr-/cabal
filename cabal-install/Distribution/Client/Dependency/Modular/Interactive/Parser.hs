module Distribution.Client.Dependency.Modular.Interactive.Parser (readExpr, Expression, Command(..) )where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import Control.Applicative hiding ((<|>), optional, empty)


type Expression = [Command]
data Command    =  AutoLog
                |  Empty
                | Auto
                | Up
                | ToTop
                | Go Int
                | Break String
                | ListBreaks
                | GoBreak String
             deriving (Read, Show)


parseNullary :: String -> Parser ()
parseNullary s = optSpaces >> string s >> optSpaces


parseUnaryString :: String -> Parser String
parseUnaryString str =
                 do _ <- optSpaces
                    _ <- string str
                    _ <- many1 space
                    s <- chars
                    _ <- optSpaces
                    return (read s)
    where chars = many1 (letter)


parseUnaryInt :: String -> Parser Int
parseUnaryInt str =
                 do _ <- optSpaces
                    _ <- string str
                    _ <- many1 space
                    d <- digits
                    _ <- optSpaces
                    return (read d)
    where digits = many1 digit

parseDigit :: Parser Int
parseDigit =
             do _ <- optSpaces
                d <- many1 digit
                _ <- optSpaces
                return (read d)

optSpaces :: Parser ()
optSpaces = optional (many1 space)

empty :: Parser ()
empty = optSpaces >> eof

--ToDo: Better parser! (I.e. parseNullary "auto" should not have "autoLog" pass)
-- failing parsers should not consume stuf..
parseCmd :: Parser Command
parseCmd =
            try (Break       <$> parseUnaryString "break")
        <|> try (ListBreaks  <$  parseNullary     "listBreaks")
        <|> try (GoBreak     <$> parseUnaryString "goBreak")
        <|> try (AutoLog     <$  parseNullary     "log")
        <|> try (Auto        <$  parseNullary     "auto")
        <|> try (Go          <$> parseUnaryInt    "go")
        <|> try (Up          <$  parseNullary     "up")
        <|> try (ToTop       <$  parseNullary     "top")
        <|> try (Go          <$> parseDigit)
        <|>       Empty       <$  empty

parseExpr :: Parser Expression
parseExpr = sepBy1 parseCmd sep
    where sep = spaces >> char ',' >> spaces

readExpr :: String -> Either String Expression
readExpr input = case parse parseExpr "" input of
    Left err -> Left $ show err
    Right val -> Right val