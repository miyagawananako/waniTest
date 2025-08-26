{-# LANGUAGE OverloadedStrings #-}
module Problems.OracleProblems (
  yes,
  notYes
)
where
import qualified DTS.DTTdeBruijn as U
import qualified ProblemBase as PB
import qualified DTS.Prover.Wani.WaniBase as B

yes :: [PB.TestType]
yes = [oracleTest1,oracleTest2]

notYes :: [PB.TestType]
notYes = [testNo]

oracleTest1 :: PB.TestType
oracleTest1 =
  let
    sigEnv = [("kareeudon",U.Entity),("yummy",U.Pi U.Entity (U.Type)),("udon",U.Pi U.Entity (U.Type)),("noodle",U.Pi U.Entity (U.Type))]
    varEnv = [U.App (U.Con "udon") (U.Con "kareeudon")]
    pre_type = U.App (U.Con "noodle") (U.Con "kareeudon")
  in (True,PB.executeWithOracle 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

oracleTest2 :: PB.TestType
oracleTest2 =
  let
    sigEnv = [("kareeudon",U.Entity),("yummy",U.Pi U.Entity (U.Type)),("udon",U.Pi U.Entity (U.Type)),("noodle",U.Pi U.Entity (U.Type))]
    varEnv = [U.Pi (U.Entity) (U.Pi (U.App (U.Con "noodle") (U.Var 0)) (U.App (U.Con "yummy") (U.Var 1))),U.App (U.Con "udon") (U.Con "kareeudon")]
    pre_type = U.App (U.Con "yummy") (U.Con "kareeudon")
  in (True,PB.executeWithOracle 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

testNo :: PB.TestType
testNo = 
  let
    sigEnv = [("kareeudon",U.Entity),("yummy",U.Pi U.Entity (U.Type)),("udon",U.Pi U.Entity (U.Type)),("noodle",U.Pi U.Entity (U.Type))]
    varEnv = [U.Pi (U.Entity) (U.Pi (U.App (U.Con "udon") (U.Var 0)) (U.App (U.Con "yummy") (U.Var 1))),U.App (U.Con "noodle") (U.Con "kareeudon")]
    pre_type = U.App (U.Con "yummy") (U.Con "kareeudon")
  in (False,PB.executeWithOracle 3 (U.ProofSearchQuery sigEnv varEnv pre_type))