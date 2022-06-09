
module Ch7 where

import qualified Control.Applicative as A
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T
import Text.Read (readMaybe)
import Control.Monad (forever)

data Term = TmVar Int
          | TmAbs Term
          | TmApp Term Term

instance Show Term where
  show (TmVar n)      = show n
  show (TmAbs t)      = "(lam. " ++ show' t ++ ")"
    where
    show' (TmAbs tm)  = "lam. " ++ show' tm
    show' tm          = show tm
  show (TmApp t1 t2)  = show t1 ++ " " ++ show t2


infixl 5 $<
($<) :: Term -> Term -> Term
($<) = TmApp

l :: Term -> Term
l = TmAbs

v :: Int -> Term
v = TmVar

testTerm :: Term
testTerm = (l$ l$ l$ v 2 $< v 1 $< v 0) $< (l$ l$ v 1 $< v 0)

testTerm' :: Term
testTerm' = (l$ l$ (l$ l$ v 1 $< v 0) $< v 1 $< v 0)

shift :: Int -> Int -> Term -> Term
shift c d (TmVar k)     = if k < c then TmVar k else TmVar $ k + d
shift c d (TmApp t1 t2) = shift c d t1 $< shift c d t2
shift c d (TmAbs t)     = TmAbs $ shift (c + 1) d t

subst :: Int -> Term -> Term -> Term
subst j s (TmVar k)   | k == j    = s
subst _ _ k@(TmVar _) | otherwise = k
subst j s (TmAbs t)               = l $ subst (j + 1) (shift 0 1 s) t
subst j s (TmApp t1 t2)           = subst j s t1 $< subst j s t2
-- 每一条rule都对应一个函数, 最终通过<|>连接起来
eAppAbs :: Term -> Maybe Term
eAppAbs (TmApp (TmAbs t) v) = Just $ shift 0 (-1) (subst 0 (shift 0 1 v) t)
eAppAbs _                   = Nothing

eAbsInner :: Term -> Maybe Term
eAbsInner (TmAbs t) = TmAbs <$> eval1 t
eAbsInner _         = Nothing

eApp1 :: Term -> Maybe Term
eApp1 (TmApp t1 v2) = TmApp <$> (eval1 t1) <*> Just v2
eApp1 _             = Nothing

eApp2 :: Term -> Maybe Term
eApp2 (TmApp t1 t2) = TmApp <$> Just t1 <*> (eval1 t2)
eApp2 _             = Nothing

isVal :: Term -> Bool
isVal (TmAbs _) = True
isVal (TmVar _) = True
isVal _         = False

-- 使用Alternative提高可读性
eval1 :: Term -> Maybe Term
eval1 t = eAppAbs t A.<|> eApp1 t A.<|> eApp2 t A.<|> eAbsInner t -- 删掉就不对函数内部进行规约

eval :: Term -> Term
eval t = case eval1 t of
  Nothing -> t
  Just t' -> eval t'

-----------------------------------------------------Parser-------------------------------------------------------------
lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

lch :: Char -> Parsec String () Char
lch = lexeme . char

lstr :: String -> Parsec String () String
lstr = lexeme . string

inParen :: Parsec String () a -> Parsec String () a
inParen p = try $ do
  _ <- char '('
  _ <- spaces -- ( 后括号的空格需要忽略
  e <- p
  _ <-spaces  -- ) 前括号的空格需要忽略
  _ <- char ')'
  return e

{-
- Term = `lam.` Term
-      | Term ` ` Term  左递归
-      | Nat
-      | `(` Term `)`

- 带运算级 TmApp 左结合
- Term = Term ` ` Term1 | Term1
- Term1= `(` Term `)` | `lam.` Term | Nat

- 左递归消除
- Term = Term1 Term'
- Term'= ` ` Term1 Term' | emp

- Term1= `(` Term `)` | `lam.` Term | Nat


- Nat = dig
-     | dig Nat
-}

pNat :: Parsec String () Int
pNat = do
  c <- many1 digit
  case readMaybe c of
    Just x -> return x
    Nothing -> A.empty

pLam :: Parsec String () Term
pLam = pTerm <* eof

pTerm :: Parsec String () Term
pTerm = try (do
    t1 <- pTerm1
    t' <- pTerm'
    case t' of
      Nothing -> return t1
      Just f  -> return $ f t1
  )

pTerm1 :: Parsec String () Term
pTerm1 = try (do
    _  <- lstr "lam."
    t  <- pTerm
    return $ TmAbs t
  ) <|> try (do
    n  <- pNat
    return $ TmVar n
  ) <|> try (do
    t  <- inParen pTerm
    return t
  )

pTerm' :: Parsec String () (Maybe (Term -> Term))
pTerm' = try (do
    _  <- many $ char ' ' <|> char '\t' <|> char '\n'
    t1 <- pTerm1
    t' <- pTerm'
    return . Just $ case t' of
      Nothing ->  \e -> TmApp e t1
      Just f  ->  \e -> f $ TmApp e t1
  ) <|> return Nothing

-- REPL
main :: IO ()
main = forever $ do
  s <- getLine
  case runParser pLam () "input" s of
    Right t  -> putStrLn . show $ eval t
    Left err -> putStrLn . show $ err

