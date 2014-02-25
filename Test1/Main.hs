module Main where

import Data.List
import ObjectType
import Attribute
import CSVCommon
import Text.ParserCombinators.Parsec.Error

samplesDir = "samples/"
objectTypesFile = samplesDir ++ "object_types.csv"
attributesFile = samplesDir ++ "attributes.csv"
attrGroupsFile = samplesDir ++ "attr_groups.csv"

printParseError :: ParseError -> IO()
printParseError x = putStrLn (show x)

readNln str ln = unlines $ take ln $ lines str
  
main = do
        contents <- readFile objectTypesFile
	either (\x -> printParseError x) (\x -> putStrLn $ (show $ processObjectTypeCSV x)) $ (parseCSV contents)
        contents1 <- readFile attributesFile
	either (\x -> printParseError x) (\x -> putStrLn $ (show $ processAttributeCSV x)) $ (parseCSV contents1)
--	either (\x -> printParseError x) (\x -> putStrLn $ (show $ processFirst x)) $ (parseCSV $ readNln contents 10)
