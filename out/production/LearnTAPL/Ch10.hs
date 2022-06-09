module Ch10 where
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad 

type Name = String
type Info = String

data Context = Context { getContext :: [(Name, Binding)] }
  deriving Show

type ContextM = MaybeT (State Context)

data Binding = NameBind | VarBind Ty
  deriving Show

infixr 9 :->
data Ty = TyBool | Ty :-> Ty
  deriving (Eq, Show)
  
-- 无错误提示 Info分量作废
data Term = TmTrue  Info
          | TmFalse Info
          | TmIf    Info Term Term Term
          | TmVar   Info Int
          | TmName  Info Name
          | TmAbs   Info Name Ty   Term
          | TmApp   Info Term Term

getBinding :: Int -> ContextM Binding
getBinding i = MaybeT . state $ \ctx -> (look i $ getContext ctx, ctx)
  where
    look :: Int -> [(Name, Binding)] -> Maybe Binding
    look _ []      = Nothing
    look 0 (b : _) = Just $ snd b
    look n (_ : c) = look (n - 1) c

addBinding :: (Name, Binding) -> ContextM ()
addBinding b = MaybeT . state $ \(Context ctx) -> (Just (), Context $ b : ctx)

popBinding :: ContextM (Name, Binding)
popBinding = MaybeT . state $ \(Context ctx) -> case ctx of
  []   -> (Nothing, Context [])
  x:xs -> (Just x, Context xs)

-- addBinding => do something => popBinding
newBinding :: (Name, Binding) -> ContextM a -> ContextM a
newBinding bind prod = do
  addBinding bind
  ret <- prod
  _   <- popBinding
  return ret

getType :: Int -> ContextM Ty
getType i = do
  bind <- getBinding i
  case bind of
    NameBind  -> empty
    VarBind t -> return t

findType :: Name -> ContextM Ty
findType name = MaybeT . state $ \(Context ctx) -> (look ctx, Context ctx)
  where
    look :: [(Name, Binding)] -> Maybe Ty
    look [] = Nothing
    look ((s, bind) : _) | s == name = case bind of
      NameBind  -> Nothing
      VarBind t -> Just t
    look (_ : xs)        | otherwise = look xs
    
type ReverseRule = Term -> ContextM Ty

tmTrue, tmFalse, tmIf, tmVar, tmAbs, tmName, tmApp :: ReverseRule
tmTrue (TmTrue _)   = MaybeT . return $ Just TyBool
tmTrue _            = empty

tmFalse (TmFalse _) = MaybeT . return $ Just TyBool
tmFalse _           = empty

tmIf (TmIf _ b t1 t2) = do
  bt  <- typeof b
  t1t <- typeof t1
  t2t <- typeof t2
  guard $ bt == TyBool && t1t == t2t
  return t1t
tmIf _ = empty

tmVar (TmVar _ i) = getType i
tmVar _           = empty

tmName (TmName _ name) = findType name
tmName _               = empty

tmAbs (TmAbs _ name ty term) = do
  newBinding (name, VarBind ty) $ do
    t <- typeof term
    return $ ty :-> t
tmAbs _ = empty

tmApp (TmApp _ t1 t2) = do
  t1t <- typeof t1
  t2t <- typeof t2
  case t1t of
    a :-> b | a == t2t -> return b
    _                  -> empty
tmApp _ = empty


typeof :: Term -> ContextM Ty
typeof term =  tmTrue   term 
           <|> tmFalse  term 
           <|> tmIf     term 
           <|> tmVar    term 
           <|> tmAbs    term 
           <|> tmName   term 
           <|> tmApp    term
infixl 7 <<
(<<) :: Term -> Term -> Term
(<<) = TmApp ""
infix 8 |:
(|:) :: Name -> Ty -> Term -> Term
(|:) = TmAbs ""
t, f :: Term
t = TmTrue ""
f = TmFalse ""
cond :: Term -> Term -> Term -> Term
cond = TmIf ""
v :: Name -> Term
v s = TmName "" s

testProg1 :: Term
testProg1= ("x"|:TyBool $ v"x") << (
    ("x"|:TyBool :-> TyBool $ "y"|:TyBool $ v"x" << v"y") 
    << ("x"|:TyBool $ 
      cond (v"x") f t)  -- not
    << f
  ) -- Just TyBool
-- runContextM (typeof testProg1) emptyContext

runContextM :: ContextM a -> Context -> (Maybe a, Context)
runContextM = runState . runMaybeT

check :: Term -> (Maybe Ty, Context)
check term = runContextM (typeof term) emptyContext

emptyContext :: Context
emptyContext = Context []






















































































































