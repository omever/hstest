module Main where

import Text.CSV
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

parse = undefined

printParseError :: ParseError -> IO()
printParseError x = putStrLn (show x)

main = do
	results <- parseCSVFromFile "object_types.csv"
	either (\x -> printParseError x) (\x -> parse x) results