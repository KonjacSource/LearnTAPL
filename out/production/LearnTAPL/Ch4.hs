module Ch4 where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc   Term
          | TmPre    Term
          | TmIsZero Term
          | Done
  deriving Show

isVal :: Term -> Bool
isVal TmTrue      = True
isVal TmFalse     = True
isVal TmZero      = True
isVal (TmSucc n)  = isVal n
isVal _           = False

eval1 :: Term -> Term
eval1 (TmSucc n)            = case eval1 n of
  Done -> Done
  n''  -> n''
eval1 (TmPre TmZero)        = TmZero
eval1 (TmPre (TmSucc n))    = n
eval1 (TmPre t)             = TmPre $ eval1 t
eval1 (TmIsZero TmZero)     = TmTrue
eval1 (TmIsZero (TmSucc _)) = TmFalse
eval1 (TmIsZero t)          = TmIsZero $ eval1 t
eval1 (TmIf TmTrue t _)     = t
eval1 (TmIf TmFalse _ t)    = t
eval1 (TmIf tb t1 t2)       = TmIf (eval1 tb) t1 t2
eval1 _                     = Done

eval :: Term -> Term
eval t = case eval1 t of 
  Done -> t
  t'   -> eval t'

testProg :: Term 
testProg = (TmIf 
  (TmFalse) 
  (TmIsZero (TmSucc TmZero)) 
  (TmSucc TmZero))
{-
 - if    ("if true then true else "false)
 - then  (is-zero (suc zero))
 - else  (suc zero)
-}