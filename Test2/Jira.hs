{-# LANGUAGE DeriveDataTypeable #-}
module Jira where

import CSVCommon
import Text.JSON
import Text.JSON.Generic

data IssueType = NewFeature
	       | Improvement
	       | Story
	       | Test
	       | Task
	       | Bug
	       | OtherIssue String
		deriving (Show, Eq, Data, Typeable)

data Status = Open
	    | Reopened
	    | InProgress
	    | Failed
	    | Started
	    | Closed
	    | CondPassed
	    | SkipTest
	    | Passed
	    | OtherStatus String
	    deriving (Show, Eq, Data, Typeable)
	
data Resolution = Unresolved
                | Resolved
                | OtherResolution String
                deriving (Show, Eq, Data, Typeable)


data JiraUser = JiraUser {
    name :: String,
    emailAddress :: String,
    displayName :: String
} deriving (Typeable, Data, Show)

data JiraLink = JiraLink {
    linktype :: String,
    linkkey :: String,
    issuetype :: String
} deriving (Show, Data, Typeable)

data JiraIssue = EmptyJira
    | JiraIssue {
	issue_type :: IssueType,
	key :: String,
	summary :: String,
	assignee :: String,
	reporter :: JiraUser,
	priority :: String,
	status :: Status,
	resolution :: Resolution,
	created :: String,
	updated :: String,
	duedate :: String,
	resolved :: String,
	linked_issues :: [JiraLink]
} deriving (Show, Data, Typeable)

instance Eq JiraIssue where
    EmptyJira == _ = False
    _ == EmptyJira = False
    x == y = (key x) == (key y)

parseIssueType "New Feature" = NewFeature
parseIssueType "Story" = Story
parseIssueType "Improvement" = Improvement
parseIssueType "Bug" = Bug 
parseIssueType "Task" = Task
parseIssueType "Test" = Test
parseIssueType s = OtherIssue s

parseIssueStatus "Open" = Open
parseIssueStatus "Reopened" = Reopened
parseIssueStatus "In Progress" = InProgress
parseIssueStatus "Passed" = Passed
parseIssueStatus "Failed" = Failed
parseIssueStatus "Started" = Started
parseIssueStatus "Closed" = Closed
parseIssueStatus "Conditionally Passed" = CondPassed
parseIssueStatus "Skip Test" = SkipTest
parseIssueStatus s = OtherStatus s

parseResolution "Unresolved" = Unresolved
parseResolution "Resolved" = Resolved
parseResolution s = OtherResolution s

emptyTicket = JiraIssue{issue_type = OtherIssue "Unknown",
                         key = "",
                         summary = "",
                         assignee = "",
                         reporter = JiraUser{name="", emailAddress="", displayName=""},
                         priority = "",
                         status = OtherStatus "Unknown",
                         resolution = OtherResolution "Unknown",
                         created = "",
                         updated = "",
                         duedate = "",
                         resolved = "",
                         linked_issues = []}

processCSV [] = error "Unable to process empty CSV"
processCSV (x:xs) = map (csvHead x) xs

csvHead (x:xs) (v:vs) = csvHead' x (csvHead xs vs) v
csvHead _ _ = emptyTicket

csvHead' :: String -> JiraIssue -> String -> JiraIssue
csvHead' "Issue Type"		d v = d{issue_type	= parseIssueType v}
csvHead' "Key"			d v = d{key       	= v}
csvHead' "Summary"		d v = d{summary      	= v}
csvHead' "Assignee"     	d v = d{assignee        = v}
csvHead' "Reporter"     	d v = d{reporter     	= JiraUser{name=v, emailAddress="", displayName=""}}
csvHead' "Priority"     	d v = d{priority        = v}
csvHead' "Status"       	d v = d{status        	= parseIssueStatus v}
csvHead' "Resolution"   	d v = d{resolution    	= parseResolution v}
csvHead' "Created"      	d v = d{created         = v}
csvHead' "Updated"      	d v = d{updated         = v}
csvHead' "Due Date"     	d v = d{duedate         = v}
csvHead' "Resolved"     	d v = d{resolved      	= v}
csvHead' "Linked Issues"	d v = d{linked_issues   = linkedIssues' v}
csvHead' _ d _ = d

linkedIssues' :: String -> [JiraLink]
linkedIssues' "" = []
linkedIssues' s = map (\x -> JiraLink{linktype="blocks", linkkey = x, issuetype=""}) $ map (filter $ (/=) ',') $ words s
