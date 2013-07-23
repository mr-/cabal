module Distribution.Client.Dependency.Modular.Interactive.Parser
            (commandList, readStatements, Statements(..), Statement(..), Selections(..), Selection(..) ) where


import           Data.List                              (sort)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

--bookmark foo ; goto aeson | parsec:test ; jump foo ; auto
--                                  ^ this is both, flag or stanza
--                     ^ while this is just package choice


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
                 | Go         Int
                 | Cut        Int
                 | Install
                 | Find       Selections
                 | Prefer     Selections
                 | ShowPlan
                 | WhatWorks
                 | Empty
                 | Back
                 deriving (Show)

data Selections =  Selections [Selection]
                         deriving (Show)

data Selection  =  SelPChoice String
                 | SelFSChoice String String
                 deriving (Show)

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
statement' =   try bsetStmt
           <|> try bjumpStmt
           <|> try blistStmt
           <|> try autoStmt
           <|> try gotoStmt
           <|> try findStmt
           <|> try upStmt
           <|> try indicateAutoStmt
           <|> try topStmt
           <|> try singleNumberStmt
           <|> try cutStmt
           <|> try goStmt
           <|> try whatWorksStmt
           <|> try installStmt
           <|> try showPlanStmt
           <|> try preferStmt
           <|> try backStmt
           <|> try emptyStmt


singleNumberStmt :: Parser Statement
singleNumberStmt =
    do  var <- lexeme integer
        return $ Go (fromInteger var)

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

goStmt :: Parser Statement
goStmt =
    do  reserved "go"
        var <- lexeme integer
        return $ Go (fromInteger var)

whatWorksStmt :: Parser Statement
whatWorksStmt =
    do  reserved "whatWorks"
        return WhatWorks

upStmt :: Parser Statement
upStmt =
    do  reserved "up"
        return Up

backStmt :: Parser Statement
backStmt =
    do  reserved "back"
        return Back

showPlanStmt :: Parser Statement
showPlanStmt =
    do  reserved "showPlan"
        return ShowPlan

indicateAutoStmt :: Parser Statement
indicateAutoStmt =
    do  reserved "indicateAuto"
        return IndicateAuto

installStmt :: Parser Statement
installStmt =
    do  reserved "install"
        return Install

topStmt :: Parser Statement
topStmt =
    do  reserved "top"
        return ToTop

autoStmt :: Parser Statement
autoStmt =
    do  reserved "auto"
        return Auto

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

blistStmt :: Parser Statement
blistStmt =
    do  reserved "blist"
        return BookList


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


readStatements :: String -> Either String Statements
readStatements input = case parse statements "" input of
    Left err ->  Left $ show err
    Right val -> Right val
