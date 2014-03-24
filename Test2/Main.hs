module Main where

import Data.List
import CSVCommon
import qualified Jira as J
import qualified JiraWeb as JW
import Text.ParserCombinators.Parsec.Error
import Text.JSON
import Control.Monad

samplesDir = ""
bugsFile = samplesDir ++ "NC.csv"
testCasesFile = samplesDir ++ "TC.csv"

printParseError :: ParseError -> IO()
printParseError x = putStrLn (show x)

-- parseCases (Right x) (Right y) = J.processCSV $ x ++ y
parseCases (Right x) (Right y) = deflateTc $ J.processCSV $ x ++ y
parseCases _ _ = error "Unable to parse files :("

deflateTc lst = deflateTc' getOnlyTestcases lst
    where getOnlyTestcases = filter isATestCase lst
          isATestCase x = J.issue_type x == J.Test

deflateTc' :: [J.JiraIssue] -> [J.JiraIssue] -> String
deflateTc' (t:ts) xs = "Test ID: "
                    ++ J.key t
                    ++ (show $ map J.key $ deflateTc'' xs (map (J.linkkey) $ J.linked_issues t) [])
                    ++ "\n\n"
                    ++ deflateTc' ts xs
deflateTc' _ _ = ""

deflateTc'' :: [J.JiraIssue] -> [String] -> [String] -> [J.JiraIssue]
deflateTc'' xs [] seen = filter entityFilter getEntities
    where getEntities = filter seenElems xs
          seenElems x = elem (J.key x) seen
          entityFilter x = (onlyBugs x
                            || onlyCRs x
                            || onlyImprovements x
                            || onlyStories x
			    || onlyTests x)
                           && onlyUnresolved x
          onlyBugs x = J.Bug == J.issue_type x
          onlyCRs x = J.NewFeature == J.issue_type x
          onlyTests x = J.Test == J.issue_type x
          onlyImprovements x = J.Improvement == J.issue_type x
          onlyStories x = J.Story == J.issue_type x
          onlyUnresolved x = J.Unresolved == J.resolution x

deflateTc'' xs ds seen = deflateTc'' xs getAllLinkedIssues (seen ++ ds)
    where
        getAllLinkedIssues = concat $ (map (\x -> map J.linkkey (J.linked_issues x)) getNotSeenElements)
        getNotSeenElements = filter notSeenElements xs
        notSeenElements x = isToSee x && isToSkip x
        isToSee x = elem (J.key x) ds
        isToSkip x = notElem (J.key x) seen

-- parseCases (Right x) (Right y) = J.processCSV $ x ++ y
parseJS (Ok x) = deflateTc (JW.issues x)
parseJS (Error s) = error $ "Unable to parse files :(" ++ s

process = do
    jiraData <- JW.getIssues
--    d <- JW.decodeJiraJS jiraData
    return jiraData --(parseJS d)

main = do
--        contents <- readFile bugsFile
--	contents' <- readFile testCasesFile
--	putStrLn $ parseCases (parseCSV contents) (parseCSV contents')
--	putStrLn "Done"
--	either (\x -> printParseError x) (\x -> putStrLn $ show $ processCSV x) (parseCSV contents)
    r <- process
    putStrLn r
