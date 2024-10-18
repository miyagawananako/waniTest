import qualified ProblemBase as PB
import qualified Problems.SimpleProblems as SP (yes,notYes) 
import qualified Problems.DifficultProblems as DP (yes,notYes)
import qualified Problems.NLPProblems as NLPP (yes,notYes)
import qualified Data.Time as TIME
import Interface.Tree (Tree(..))

testCaseList :: [PB.TestType]
testCaseList = SP.yes ++ SP.notYes ++ DP.yes ++ DP.notYes ++ NLPP.yes ++ NLPP.notYes

test :: Bool
test = PB.checkTests testCaseList

main :: IO()
main = do
  start <- TIME.getCurrentTime
  let isPass = test
  putStr $ if isPass then "passed" else "failed"
  end <- TIME.getCurrentTime
  putStrLn $ " with " ++ (show $TIME.diffUTCTime end start)
  if isPass then do
    let searchResultList = PB.getProofSearchResult testCaseList
    let searchResults = concat searchResultList
    let pairList = map (\tree -> (node tree, ruleName tree)) searchResults
    print pairList
  else do print "no proofSearchResult"