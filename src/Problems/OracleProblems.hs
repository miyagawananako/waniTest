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
yes = [oracleTest1,oracleTest2,q619,q623',q624]

notYes :: [PB.TestType]
notYes = [testNo,q623]

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

{--
a. [[A professor] or [an assistant professor]]i will attend the meeting of the university board.
b. She will report to the faculy.

[[x:entity, prof(x)],wAtM(pi1u)] + [[x:entity, aprof(x)],wAtM(pi1u)] \vdash ? : [x:entity,female(x)]

a. pf : u : x : [entity,prof(x)] →female(π1u)
b. af : u : x : [entity,aProf(x)] →female(π1u)

623 
[A professor]1 attended the meeting or the meeting was cancelled.
# She1 will report to the faculty.
--}


profsigEs = [
  ("female",U.Pi U.Entity U.Type),
  ("prof",U.Pi U.Entity U.Type),
  ("aProf",U.Pi U.Entity U.Type),
  ("wAtM",U.Pi U.Entity U.Type),
  ("mCancell",U.Type)
  ]

q619 :: PB.TestType 
q619 =
  let
    sigEnv = profsigEs
    varEnv = [U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "aProf") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0))))]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (True,PB.executeWithEFQOracle 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

q623 :: PB.TestType
q623 =
  let
    sigEnv = profsigEs
    varEnv = [U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Con "mCancell")]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (False,PB.executeWithEFQOracle 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

q623' :: PB.TestType
q623' =
  let
    sigEnv = profsigEs
    varEnv = [U.Not ((U.Con "mCancell")), U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Con "mCancell")]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (True,PB.executeWithEFQOracle 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

-- {--
-- a. Either it’s a holiday or [a customer]1 will come in.
-- b. And if it’s not a holiday, they1 will want to be served.

-- v : holoday(today) + [[x:entity,customer(x)],WCI(pi1u)], w : not holoday(today) \vdash ? [x: entity, human(x)]

-- a. cf : u : x : [entity,customer(x)] →human(π1u)
-- --}

cussigEs = [
  ("holiday",U.Pi U.Entity U.Type),
  ("customer",U.Pi U.Entity U.Type),
  ("WCI",U.Pi U.Entity U.Type),
  ("today",U.Entity),
  ("human",U.Pi U.Entity U.Type)
  ]

q624 :: PB.TestType
q624 =
  let
    sigEnv = cussigEs
    varEnv = [U.Not (U.App (U.Con "holiday") (U.Con "today")),U.Disj (U.App (U.Con "holiday") (U.Con "today")) (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "customer") (U.Var 0))) (U.App (U.Con "WCI") (U.Proj (U.Fst) (U.Var 0))))]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "human") (U.Var 0))
  in (True,PB.executeWithEFQOracle 9 (U.ProofSearchQuery sigEnv varEnv pre_type))