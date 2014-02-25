-----------------------------------------------------------------------------
--
-- Module      :  Attribute
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

module Attribute (
    processAttributeCSV
) where

import CSVCommon

data Attribute = Attribute {
    --"ATTR_ID","ATTR_TYPE_ID","ATTR_TYPE_DEF_ID","ATTR_GROUP_ID","ATTR_SCHEMA_ID","NAME","ATTR_ACCESS_TYPE","ISMULTIPLE","ISEXTGENERATED","ISEXTSTORED",
    --"ADAPTERNAME","PARAMS","UNIQUE_LEVEL","SHOW_ORDER","SHOW_HISTORY","ISSEARCHABLE","MASK","DEF_VALUE","FLAGS","DESCRIPTION","PROPERTIES","RULES",
    -- "TOOLTIP","AV_ADAPTER_NAME","AV_ADAPTER_PROPERTIES","INTERNAL_NAME"
    attr_id :: Integer,
    attr_type_id :: Integer,
    attr_type_def_id :: Integer,
    attr_group_id :: Integer,
    attr_schema_id :: Integer,
    name :: String,
    attr_access_type :: Integer,
    ismultiple :: Bool,
    isextgenerated :: Bool,
    isextstored :: Bool,
    adaptername :: String,
    params :: String,
    unique_level :: String,
    show_order :: Integer,
    show_history :: Integer,
    issearchable :: Bool,
    mask :: String,
    def_value :: String,
    flags :: Integer,
    description :: String,
    properties :: String,
    rules :: String,
    tooltip :: String,
    av_adapter_name :: String,
    av_adapter_properties :: String,
    internal_name :: String
} deriving (Show)

emptyAttribute = Attribute {
    attr_id = 0,
    attr_type_id = 0,
    attr_type_def_id = 0,
    attr_group_id = 0,
    attr_schema_id = 0,
    name = "",
    attr_access_type = 0,
    ismultiple = False,
    isextgenerated = False,
    isextstored = False,
    adaptername = "",
    params = "",
    unique_level = "",
    show_order = 0,
    show_history = 0,
    issearchable = False,
    mask = "",
    def_value = "",
    flags = 0,
    description = "",
    properties = "",
    rules = "",
    tooltip = "",
    av_adapter_name = "",
    av_adapter_properties = "",
    internal_name = ""
}

processAttributeCSV [] = error "Unable to process empty CSV"
processAttributeCSV (x:xs) = map (csvHead x) xs

csvHead (x:xs) (v:vs) = csvHead' x (csvHead xs vs) v
csvHead _ _ = emptyAttribute

--"ATTR_ID","ATTR_TYPE_ID","ATTR_TYPE_DEF_ID","ATTR_GROUP_ID","ATTR_SCHEMA_ID","NAME","ATTR_ACCESS_TYPE","ISMULTIPLE","ISEXTGENERATED","ISEXTSTORED",
--"ADAPTERNAME","PARAMS","UNIQUE_LEVEL","SHOW_ORDER","SHOW_HISTORY","ISSEARCHABLE","MASK","DEF_VALUE","FLAGS","DESCRIPTION","PROPERTIES","RULES",
-- "TOOLTIP","AV_ADAPTER_NAME","AV_ADAPTER_PROPERTIES","INTERNAL_NAME"
csvHead' :: String -> Attribute -> String -> Attribute
csvHead' "ATTR_ID"                  d v = d{attr_id = readInt v}
csvHead' "ATTR_TYPE_ID"             d v = d{attr_type_id = readInt v}
csvHead' "ATTR_TYPE_DEF_ID"         d v = d{attr_type_def_id = readInt v}
csvHead' "ATTR_GROUP_ID"            d v = d{attr_group_id = readInt v}
csvHead' "ATTR_SCHEMA_ID"           d v = d{attr_schema_id = readInt v}
csvHead' "NAME"                     d v = d{name = v}
csvHead' "ATTR_ACCESS_TYPE"         d v = d{attr_access_type = readInt v}
csvHead' "ISMULTIPLE"               d v = d{ismultiple = readBool v}
csvHead' "ISEXTGENERATED"           d v = d{isextgenerated = readBool v}
csvHead' "ISEXTSTORED"              d v = d{isextstored = readBool v}
csvHead' "ADAPTERNAME"              d v = d{adaptername = v}
csvHead' "PARAMS"                   d v = d{params = v}
csvHead' "UNIQUE_LEVEL"             d v = d{unique_level = v}
csvHead' "SHOW_ORDER"               d v = d{show_order = readInt v}
csvHead' "SHOW_HISTORY"             d v = d{show_history = readInt v}
csvHead' "ISSEARCHABLE"             d v = d{issearchable = readBool v}
csvHead' "MASK"                     d v = d{mask = v}
csvHead' "DEF_VALUE"                d v = d{def_value = v}
csvHead' "FLAGS"                    d v = d{flags = readInt v}
csvHead' "DESCRIPTION"              d v = d{description = v}
csvHead' "PROPERTIES"               d v = d{properties = v}
csvHead' "RULES"                    d v = d{rules = v}
csvHead' "TOOLTIP"                  d v = d{tooltip = v}
csvHead' "AV_ADAPTER_NAME"          d v = d{av_adapter_name = v}
csvHead' "AV_ADAPTER_PROPERTIES"    d v = d{av_adapter_properties = v}
csvHead' "INTERNAL_NAME"            d v = d{internal_name = v}
csvHead' _ d _ = d
