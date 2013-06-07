module Distribution.Client.Interactive.Parser (readExpr, Expression, Command(..) )where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import Control.Applicative hiding ((<|>), optional)

type Expression = [Command]
data Command =  Empty | Auto | Up | ToTop | Go Int deriving (Read, Show)


parseNullary :: String -> Parser String
parseNullary = string 

parseUnary :: String -> Parser Int
parseUnary str = do _ <- optSpace
                    _ <- string str
                    _ <- many1 space 
                    d <- digits
                    _ <- optSpace
                    return (read d)
    where digits = many1 digit

parseDigit :: Parser Int
parseDigit = do _ <- optSpace
                d <- many1 digit
                _ <- optSpace
                return (read d)

optSpace = optional (many1 space)

noting = do _ <- optSpace
            eof


parseCmd :: Parser Command 
parseCmd =  Auto    <$ parseNullary "auto"
        <|> Go      <$> parseUnary "go"
        <|> Up      <$ parseNullary "up"
        <|> ToTop   <$ parseNullary "top"
        <|> Go      <$> parseDigit
        <|> Empty   <$ noting

parseExpr :: Parser Expression
parseExpr = sepBy1 parseCmd sep
    where sep = spaces >> char ',' >> spaces 

readExpr :: String -> Either String Expression
readExpr input = case parse parseExpr "" input of
    Left err -> Left $ show err
    Right val -> Right val