module Main where

import System.Environment
import LispParser
import LispVals
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head 
