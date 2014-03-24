-----------------------------------------------------------------------------
--
-- Module      :  JiraWeb
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
{-# LANGUAGE DeriveDataTypeable #-}
module JiraWeb where

import Text.JSON
import Text.JSON.Generic
import Jira
import Network.HTTP
import Network.HTTP.Auth

filename = "test.json"

data SearchResponse = SearchResponse {
    startAt :: Integer,
    maxResults :: Integer,
    total :: Integer,
    issues :: [JiraIssue]
} deriving (Typeable, Data, Show)

instance JSON SearchResponse where
    readJSON (JSObject s) = do
        startAt <- s ! "startAt"
        maxResults <- s ! "maxResults"
        total <- s ! "total"
        issues <- valFromObj "issues" s
        return SearchResponse {startAt = startAt, maxResults = maxResults, total = total, issues = issues}
      where (!) = flip valFromObj
    readJSON _ = error "Non object input"
    showJSON = undefined

instance JSON JiraIssue where
    readJSON (JSObject s) = do
        key <- valFromObj "key" s
        f <- valFromObj "fields" s
        summary <- valFromObj "summary" f
        issuetypeobj <- valFromObj "issuetype" f
        issuetype <- valFromObj "name" issuetypeobj
        resolution <- valFromObj "resolution" f
        reporter <- valFromObj "reporter" f
        priority <- valFromObj "priority" f
        duedate <- valFromObj "duedate" f
        statusObj <- valFromObj "status" f
        status <- valFromObj "name" statusObj
        links <- valFromObj "issuelinks" f
        return emptyTicket {key = key
                        , summary = summary
                        , issue_type = parseIssueType issuetype
                        , resolution = parseResolution $ toMaybeS resolution
                        , reporter = reporter
                        , priority = toMaybeS priority
                        , duedate = toMaybeS duedate
                        , status = parseIssueStatus status
                        , linked_issues = links
                        }
    showJSON = undefined

instance JSON JiraUser where
    readJSON (JSObject s) = do
        name <- valFromObj "name" s
        emailAddress <- valFromObj "emailAddress" s
        displayName <- valFromObj "displayName" s
        return JiraUser {name = name, emailAddress = emailAddress, displayName = displayName}
    showJSON = undefined

instance JSON JiraLink where
    readJSON (JSObject s) = do
        typeobj <- valFromObj "type" s
        linktype <- valFromObj "outward" typeobj
        outwardIssue <- valFromObj "outwardIssue" s
        linkkey <- valFromObj "key" outwardIssue
        issueTypeO <- valFromObj "issuetype" outwardIssue
        issuetype <- valFromObj "name" issueTypeO
        return JiraLink{linktype = linktype, linkkey = linkkey, issuetype = issuetype}
    showJSON = undefined

toMaybeS (JSNull) = ""
toMaybeS (JSString a) = fromJSString a

decodeJiraJS :: String -> Result SearchResponse
decodeJiraJS s = decode s

getIssues = getIssues' 0 ""

getIssues' n r = do
    tryGet <- simpleHTTP $ (getRequest $ "https://jira.symbioss.sk/rest/api/2/search?jql=project=NC;startAt=" ++ (show n))
    getResponseBody tryGet
