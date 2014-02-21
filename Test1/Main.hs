module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

data ObjectType = ObjectType {
	object_type_id :: Int,
	parent_id :: Int,
	picture_id :: Int,
	name :: String,
	description :: String,
	isclass :: Bool,
	issystem :: Bool,
	issearchable :: Bool,
	icon_id :: Int,
	alias :: String,
	flags :: Int,
	properties :: String,
	isabstract :: Bool,
	internal_name :: String
}

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

samplesDir = "samples/"
objectTypesFile = samplesDir ++ "object_types.csv"

printParseError :: ParseError -> IO()
printParseError x = putStrLn (show x)

main = do
        contents <- readFile objectTypesFile
	either (\x -> printParseError x) (\x -> putStrLn $ show x) $ (parseCSV contents)
