module Distribution.Client.Dependency.Modular.Interactive.Parser
            (readStatements, Statements(..), Statement(..), Selections(..), Selection(..) ) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--bookmark foo ; goto aeson | parsec:test ; jump foo ; auto
--                                  ^ this is both, flag or stanza
--                     ^ while this is just package choice


data Statements =  Statements [Statement]
                 deriving (Show)
data Statement  =  BookSet    String
                 | BookJump   String
                 | BookList
                 | Auto
                 | AutoLog
                 | Goto       Selections
                 | Up
                 | ToTop
                 | Go         Int
                 deriving (Show)

data Selections =  Selections [Selection]
                         deriving (Show)

data Selection  =  SelPChoice String
                 | SelFSChoice String String
                 deriving (Show)


languageDef =
  emptyDef { Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "bset"
                                     , "bjump"
                                     , "blist"
                                     , "auto"
                                     , "goto"
                                     , "up"
                                     , "top"
                                     , "go"
                                     , "autoLog"
                                     ]
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
                    do  list <- (sepBy1 statement' semi)
                        return $ Statements list

statement' :: Parser Statement
statement' =   try bsetStmt
           <|> try bjumpStmt
           <|> try blistStmt
           <|> try autoLogStmt
           <|> try autoStmt
           <|> try gotoStmt
           <|> try upStmt
           <|> try topStmt
           <|> try goStmt


gotoStmt :: Parser Statement
gotoStmt =
    do  reserved "goto"
        sel <- selectionsParser
        return $ Goto sel

goStmt :: Parser Statement
goStmt =
    do  reserved "go"
        var <- lexeme integer
        return $ Go (fromInteger var)

upStmt :: Parser Statement
upStmt =
    do  reserved "up"
        return ToTop

topStmt :: Parser Statement
topStmt =
    do  reserved "top"
        return ToTop

autoLogStmt :: Parser Statement
autoLogStmt =
    do  reserved "autoLog"
        return Auto

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