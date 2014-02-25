-----------------------------------------------------------------------------
--
-- Module      :  ObjectType
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  omever@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ObjectType (
    processObjectTypeCSV
) where

import CSVCommon

data ObjectType = ObjectType {
	object_type_id :: Integer,
	parent_id :: Integer,
	picture_id :: Integer,
	name :: String,
	description :: String,
	isclass :: Bool,
	issystem :: Bool,
	issearchable :: Bool,
	icon_id :: String,
	alias :: String,
	flags :: Integer,
	properties :: String,
	isabstract :: Bool,
	internal_name :: String
} deriving (Show)

emptyObjectType = ObjectType{object_type_id = 0,
                         parent_id = 0,
                         picture_id = 0,
                         name = "",
                         description = "",
                         isclass = False,
                         issystem = False,
                         issearchable = False,
                         icon_id = "",
                         alias = "",
                         flags = 0,
                         properties = "",
                         isabstract = False,
                         internal_name = ""}

processObjectTypeCSV [] = error "Unable to process empty CSV"
processObjectTypeCSV (x:xs) = map (csvHead x) xs

csvHead (x:xs) (v:vs) = csvHead' x (csvHead xs vs) v
csvHead _ _ = emptyObjectType

csvHead' :: String -> ObjectType -> String -> ObjectType
csvHead' "OBJECT_TYPE_ID"   d v = d{object_type_id = readInt v}
csvHead' "PARENT_ID"        d v = d{parent_id = readInt v}
csvHead' "PICTURE_ID"       d v = d{picture_id = readInt v}
csvHead' "NAME"             d v = d{name = v}
csvHead' "DESCRIPTION"      d v = d{description = v}
csvHead' "ISCLASS"          d v = d{isclass = readBool v}
csvHead' "ISSYSTEM"         d v = d{issystem = readBool v}
csvHead' "ISSEARCHABLE"     d v = d{issearchable = readBool v}
csvHead' "ICON_ID"          d v = d{icon_id = v}
csvHead' "ALIAS"            d v = d{alias = v}
csvHead' "FLAGS"            d v = d{flags = readInt v}
csvHead' "PROPERTIES"       d v = d{properties = v}
csvHead' "ISABSTRACT"       d v = d{isabstract = readBool v}
csvHead' "INTERNAL_NAME"    d v = d{internal_name = v}
csvHead' _ d _ = d

