{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

class Equal (a :: Nat) (b :: Nat) | a -> b 
instance Equal a a 

data Nat = Zero | Succ Nat

type family (a :: Nat) :+ (b :: Nat) where
  'Zero    :+ n = n
  ('Succ m) :+ n = 'Succ (m :+ n)


g :: Equal ('Succ n) ('Succ 'Zero) => ()
g = ()

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2


data Void
type Not p = p -> Void

prop1 :: ((p, q) -> r) -> ((p, Not r) -> Not q)
prop1 f (p, not_r) q = not_r (f (p, q))

add :: Num a => a -> a -> a
add = (+)


