module LispVals where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "("  ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- ====== ERRORS ======

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected 
                                            ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

