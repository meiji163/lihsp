module LispParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad 
import Control.Monad.Except
import LispVals

-- ===== PARSER =====
-- Parsec Notes: 
--      <|> parses left first, only matches right AFTER consuming chars  

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ chars 
                char '"'
                return $ String x
        where 
            chars = escaped <|> noneOf "\""
            escaped = choice $ map tryEscaped escapedChars
            tryEscaped (x,y)  = try $ char '\\' >> char x >> return y 
            escapedChars = zip "bnfrt\\\"/" "\b\n\f\r\t\\\"/"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
    x <- many1 digit
    return $ Number (read x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do 
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- String -> Either LispError Lispval
readExpr :: String -> ThrowsError LispVal 
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val 
