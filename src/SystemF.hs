{-# LANGUAGE UnicodeSyntax #-}
module SystemF where
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type Id = String
type Info = String

infixr 2 :->
-- types
data Ty = TyVar Id | Ty :-> Ty | Forall Id Ty | IntTy
  deriving (Eq, Show)

infixl 9 :$, :@
data Term
  = Var Id
  | Lam Id Ty Term
  | Term :$ Term
  | TLam Id Term    -- type lambda, given a type return a term
  | Term :@ Ty      -- type apply, result is a term
--  | Let Id {- = -} Term {- in -} Term
  | Lit Int
  deriving (Show)

type Ctx = [(Id, Bind)]

-- the Id has its Bind, shows its Type or Kind, which must be *.
data Context = Context { getCtx :: Ctx }

-- Id is one of those two cases
data Bind = TypeBind Ty | KindBind -- :: *

-- ContextM a ~= Context -> (Either Info a, Context)
type ContextM = ExceptT Info (State Context)

context :: (s -> (Either e a, s)) -> ExceptT e (State s) a
context = ExceptT . state

wrong :: Info -> ContextM a
wrong info = context $ \ctx -> (Left $ info ++ "\n", ctx)

getBind :: Id -> ContextM Bind
getBind name = context $ \(Context ctx) -> case helper ctx of
    Left info  -> (Left info, Context ctx)
    Right bind -> (Right bind, Context ctx)
  where helper :: Ctx -> Either Info Bind
        helper [] = Left $ "unknow identifier \"" ++ name ++ "\""
        helper ((i, t) : xs)
          | i == name = Right t
          | otherwise = helper xs

pushBind :: (Id, Bind) -> ContextM ()
pushBind b = context $ \(Context ctx) -> (Right (), Context $ b : ctx)

popBind :: ContextM (Id, Bind)
popBind = context $ \(Context ctx) -> case ctx of
  []       -> (Left "nothing in context", Context ctx)
  (x : xs) -> (Right x, Context xs)

-- pushBind => do something => popBind
withContext :: (Id, Bind) -> ContextM a -> ContextM a
withContext bind prod = do
  pushBind bind
  ret <- prod
  _   <- popBind
  return ret

-- tySubst x t t' , substitute x in t' with t
tySubst :: Id -> Ty -> Ty -> Ty
tySubst name t (TyVar x)     | name == x = t
                             | otherwise = TyVar x
tySubst name t (t1 :-> t2)   = tySubst name t t1 :-> tySubst name t t2
tySubst name t (Forall x t1) | name == x = Forall x t1 -- shadowed
                             | otherwise = Forall x $ tySubst name t t1
tySubst _ _ IntTy = IntTy

type TyRule = Term -> ContextM Ty

tInt :: TyRule
tInt (Lit _) = return IntTy
tInt _       = empty

tVar :: TyRule
tVar (Var name) = do
  bind <- getBind name
  case bind of
    TypeBind t -> return t
    KindBind   -> empty
tVar _ = empty

tAbs :: TyRule
tAbs (Lam x t term) = withContext (x, TypeBind t) $ do
  rt <- typeof term
  return $ t :-> rt
tAbs _ = empty

tApp :: TyRule
tApp (f :$ x) = do
  ft <- typeof f
  xt <- typeof x
  case ft of
    a :-> b | a == xt   -> return b
    t       | otherwise -> wrong $ "in term " ++ show f ++ "; expect function type, got " ++ show t ++ " instead"
tApp _ = empty

tTAbs :: TyRule
tTAbs (TLam t term) = withContext (t, KindBind) $ do
  tt <- typeof term
  return $ Forall t tt
tTAbs _ = empty

tTApp :: TyRule
tTApp (term :@ ty) = do
  termT <- typeof term
  case termT of
    Forall x t -> return $ tySubst x ty t
    t          -> wrong $ "in term " ++ show term ++ "; expect forall type, got " ++ show t ++ " instead"
tTApp _ = empty

tUndefined :: TyRule
tUndefined _ = wrong "# rule exhausted #"

typeof :: Term -> ContextM Ty
typeof t
  =  tVar  t
 <|> tInt  t
 <|> tAbs  t
 <|> tApp  t
 <|> tTAbs t
 <|> tTApp t
 <|> tUndefined t

-- test1 : (Int -> Int) -> Int = λ x: Int -> Int. x 1
test1 :: Term
test1 = Lam "x" (IntTy :-> IntTy) $ Var "x" :$ Lit 1

-- test2 : ∀ T. T -> T = Λ T. λ x: T. x
test2 :: Term
test2 = TLam "T" $ Lam "x" (TyVar "T") $ Var "x"

-- test3 = Λ T. λ x: Int. x :@ T
test3 :: Term
test3 = TLam "T" $ Lam "x" IntTy $ Var "x" :@ TyVar "T"

printType :: Term -> IO ()
printType term = do
  let (r, _) = runState (runExceptT $ typeof term) $ Context [] :: (Either Info Ty, Context)
  case r of
    Left err -> putStrLn "error:"   >> putStrLn err
    Right ty -> putStrLn "succeed:" >> putStrLn (show ty)

