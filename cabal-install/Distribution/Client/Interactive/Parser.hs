module Distribution.Client.Interactive.Parser (readExpr, Expression, Command(..) )where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import Control.Applicative hiding ((<|>))

type Expression = [Command]
data Command =  Auto | Up | ToTop | Go Int deriving (Read, Show)


parseNullary :: String -> Parser String
parseNullary = string 

parseUnary :: String -> Parser Int
parseUnary str = do _ <- string str
                    _ <- many1 space 
                    d <- digits
                    return (read d)
    where digits = many1 digit

parseCmd :: Parser Command 
parseCmd =  Auto  <$ parseNullary "auto"
        <|> Go  <$> parseUnary "go"
        <|> Up  <$ parseNullary "up"
        <|> ToTop <$ parseNullary "top"

parseExpr :: Parser Expression
parseExpr = sepBy1 parseCmd sep
    where sep = spaces >> char ',' >> spaces 

readExpr :: String -> Either String Expression
readExpr input = case parse parseExpr "" input of
    Left err -> Left $ show err
    Right val -> Right val