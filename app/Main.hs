import qualified ProblemBase as PB
import qualified Problems.SimpleProblems as SP (yes,notYes) 
import qualified Problems.DifficultProblems as DP (yes,notYes)
import qualified Problems.NLPProblems as NLPP (yes,notYes)
import qualified Data.Time as TIME
import Interface.Tree (Tree(..))
import Data.Store (encode)
import qualified Data.ByteString as B --bytestring

saveFilePath :: FilePath
saveFilePath = "app/proofSearchResult.txt"

main :: IO()
main = do
  start <- TIME.getCurrentTime
  test <- PB.checkTests (SP.yes ++ SP.notYes ++ DP.yes ++ DP.notYes ++ NLPP.yes ++ NLPP.notYes)
  putStr $ if test then "passed" else "failed"
  end <- TIME.getCurrentTime
  putStrLn $ " with " ++ (show $TIME.diffUTCTime end start)

  searchResults <- PB.getProofSearchResult (SP.yes ++ DP.yes ++ NLPP.yes)
  let pairList = map (\tree -> (node tree, ruleName tree)) (concat searchResults)
  B.writeFile saveFilePath (encode pairList)