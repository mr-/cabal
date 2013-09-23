module Distribution.Client.Dependency.Modular.Interactive.Parser
            (commandList, readStatements, Statements(..), Statement(..), Selections(..), Selection(..), GoChoice(..) ) where

import           Control.Applicative                    ((<$>))
import           Data.List                              (sort)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- bsetfoo ; goto aeson | parsec:test ; bjump foo ; auto
--                  ^           ^ this is both, flag or stanza
--                  | while this is just package choice


data Statements =  Statements [Statement]
                 deriving (Show)
data Statement  =  BookSet    String
                 | BookJump   String
                 | BookList
                 | Auto
                 | IndicateAuto
                 | Goto       Selections
                 | Up
                 | ToTop
                 | Go         GoChoice
                 | Cut        Int
                 | Install
                 | Find       Selections
                 | Prefer     Selections
                 | ShowPlan
                 | WhatWorks
                 | Empty
                 | Back
                 | ShowHistory
                 | FailReason
                 | Help
                 deriving (Show, Eq)

data Selections =  Selections [Selection]
                         deriving (Show, Eq)

data Selection  =  SelPChoice String
                 | SelFSChoice String String
                 deriving (Show, Eq)

data GoChoice = Package String
              | Version String
              | Number  Integer
              deriving (Show, Eq)
commandList :: [String]
commandList = sort
           [ "bset"
           , "bjump"
           , "blist"
           , "auto"
           , "goto"
           , "up"
           , "top"
           , "go"
           , "install"
           , "cut"
           , "indicateAuto"
           , "showPlan"
           , "prefer"
           , "whatWorks"
           , "back"
           , "showHistory"
           , "failReason"
           , "help"
           ]

languageDef =
  emptyDef { Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> oneOf "-_" --is that enough?
           , Token.reservedNames   = commandList
           , Token.reservedOpNames = ["|", ":"]
           }

lexer = Token.makeTokenParser languageDef

identifier  = Token.identifier  lexer -- parses an identifier
reserved    = Token.reserved    lexer -- parses a reserved name
reservedOp  = Token.reservedOp  lexer -- parses an operator
integer     = Token.integer     lexer -- parses an integer
semi        = Token.semi        lexer -- parses a semicolon
whiteSpace  = Token.whiteSpace  lexer -- parses whitespace
lexeme      = Token.lexeme      lexer


statements :: Parser Statements
statements = whiteSpace >> sequenceOfStmt
    where sequenceOfStmt =
                    do  list <- sepBy1 statement' semi
                        return $ Statements list

statement' :: Parser Statement
statement' =   try (nullAry "blist" BookList)
           <|> try (nullAry "auto"  Auto)
           <|> try (nullAry "up" Up)
           <|> try (nullAry "indicateAuto" IndicateAuto)
           <|> try (nullAry "whatWorks" WhatWorks)
           <|> try (nullAry "install"   Install)
           <|> try (nullAry "showPlan" ShowPlan)
           <|> try (nullAry "back" Back)
           <|> try (nullAry "showHistory" ShowHistory)
           <|> try (nullAry "failReason" FailReason)
           <|> try (nullAry "help" Help)
           <|> try (nullAry "top" ToTop)
           <|> try bsetStmt
           <|> try bjumpStmt
           <|> try gotoStmt
           <|> try findStmt
           <|> try cutStmt
           <|> try preferStmt
           <|> try emptyStmt
           <|> try goStmt           -- Order is important here.
           <|> try singleNumberStmt -- These need to come last,
                                    -- in that order!

nullAry :: String -> Statement -> Parser Statement
nullAry str st =
    do  reserved str
        return st

singleNumberStmt :: Parser Statement
singleNumberStmt =
    do  var <- goParser
        return $ Go var

emptyStmt :: Parser Statement
emptyStmt =
    do  eof
        return Empty

findStmt :: Parser Statement
findStmt =
    do  reserved "find"
        sel <- selectionsParser
        return $ Find sel

gotoStmt :: Parser Statement
gotoStmt =
    do  reserved "goto"
        sel <- selectionsParser
        return $ Goto sel

preferStmt :: Parser Statement
preferStmt =
    do reserved "prefer"
       sel <- selectionsParser
       return $ Prefer sel

cutStmt :: Parser Statement
cutStmt =
    do  reserved "cut"
        var <- lexeme integer
        return $ Cut (fromInteger var)

bsetStmt :: Parser Statement
bsetStmt =
    do  reserved "bset"
        var  <- identifier
        return $ BookSet var

bjumpStmt :: Parser Statement
bjumpStmt =
    do  reserved "bjump"
        var  <- identifier
        return $ BookJump var

goStmt :: Parser Statement
goStmt =
    do reserved "go"
       foo <- goParser
       return $ Go foo



selectionsParser :: Parser Selections
selectionsParser =
    do  list <- sepBy1 selectionParser (lexeme (char '|'))
        return $ Selections list

selectionParser :: Parser Selection
selectionParser =   try fsSelection
                <|> try packageSelection

packageSelection :: Parser Selection
packageSelection =
    do  package <- identifier
        return $ SelPChoice package

fsSelection :: Parser Selection
fsSelection =
    do  package <- identifier
        _       <- lexeme (char ':')
        flag    <- identifier
        return $ SelFSChoice package flag

-- data GoChoice = Package String
--               | Version String
--               | Number  Int
--               deriving (Show, Eq)


goParser :: Parser GoChoice
goParser = try goNumber
       <|> try goVersion
       <|> try goPackage


goVersion :: Parser GoChoice
goVersion =
  do version <- many1 (choice [string ".", show <$> integer])
     return $ Version $ concat version

goNumber :: Parser GoChoice
goNumber =
  do _  <- try $ optional (char '*')
     nr <- integer
     notFollowedBy (char '.')
     _  <- many (choice [char 'F', char 'I'])
     return $ Number nr

goPackage :: Parser GoChoice
goPackage =
  do package <- many1 (choice [many1 letter, string "-"])
     return $ Package $ concat package

readStatements :: String -> Either String Statements
readStatements input = case parse statements "" input of
    Left err ->  Left $ show err
    Right val -> Right val
