module Distribution.Client.Dependency.Modular.Interactive.Parser (readExpr, Expression, Command(..) )where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import Control.Applicative hiding ((<|>), optional, empty)

type Expression = [Command]
data Command =  AutoLog | Empty | Auto | Up | ToTop | Go Int deriving (Read, Show)


parseNullary :: String -> Parser ()
parseNullary s = optSpaces >> string s >> optSpaces

parseUnary :: String -> Parser Int
parseUnary str = do _ <- optSpaces
                    _ <- string str
                    _ <- many1 space
                    d <- digits
                    _ <- optSpaces
                    return (read d)
    where digits = many1 digit

parseDigit :: Parser Int
parseDigit = do _ <- optSpaces
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
parseCmd =  AutoLog <$ parseNullary "log"
        <|> Auto    <$ parseNullary "auto"
        <|> Go      <$> parseUnary "go"
        <|> Up      <$ parseNullary "up"
        <|> ToTop   <$ parseNullary "top"
        <|> Go      <$> parseDigit
        <|> Empty   <$ empty

parseExpr :: Parser Expression
parseExpr = sepBy1 parseCmd sep
    where sep = spaces >> char ',' >> spaces

readExpr :: String -> Either String Expression
readExpr input = case parse parseExpr "" input of
    Left err -> Left $ show err
    Right val -> Right val