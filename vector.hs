{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Vector where

import Data.Monoid

data EmptyVec value = EmptyVec
  deriving Show
  
data Append vec value = Append { getVec :: vec value, getValue :: value }
  deriving Show

instance Functor EmptyVec where
  fmap = (const . const) EmptyVec

instance Applicative EmptyVec where
  pure a = EmptyVec
  f <*> v = EmptyVec

instance Functor vec => Functor (Append vec) where
  fmap f t = Append (fmap f $ getVec t) (f $ getValue t)

instance Applicative vec => Applicative (Append vec) where
  pure a = Append (pure a) a
  f <*> v = Append (getVec f <*> getVec v) (getValue f $ getValue v)

instance Foldable (Append EmptyVec) where
  foldMap f v = f (getValue v)

instance Foldable (Append vec) => Foldable (Append (Append vec)) where
  foldMap f v = (foldMap f (getVec v)) <> f (getValue v)

type Vec1D a = Append EmptyVec a
type Vec2D a = Append (Append EmptyVec) a -- type synonyms don't get curried?
type Vec3D a = Append (Append (Append EmptyVec)) a

(#) = Append
infixl 4 #

-- TODO:
-- zero vector

-- TODO
-- implement as a typeclass

(.+) :: (Floating a, Applicative vec) => Append vec a -> Append vec a -> Append vec a
infixl 6 .+
a .+ b = (+) <$> a <*> b

(.-) :: (Floating a, Applicative vec) => Append vec a -> Append vec a -> Append vec a
infixl 6 .-
a .- b = (-) <$> a <*> b

vabs :: (Floating a, Applicative vec, Foldable (Append vec)) => Append vec a -> a
vabs a = sqrt . getSum . foldMap Sum $ (**2) <$> a

(.*) :: (Floating a, Applicative vec) => a -> Append vec a -> Append vec a
infixl 7 .*
k .* a = (k*) <$> a

(./) :: (Floating a, Applicative vec) => Append vec a -> a -> Append vec a
infixl 7 ./
a ./ k = (/k) <$> a

unit :: (Foldable (Append vec), Applicative vec, Floating a) => Append vec a -> Append vec a
unit vec = vec ./ vabs vec

-- величина проекции a на b
proj :: (Foldable (Append vec), Applicative vec, Floating a) => Append vec a -> Append vec a -> a
proj a b = scalar a b / vabs b

scalar :: (Foldable (Append vec), Applicative vec, Num a) => Append vec a -> Append vec a -> a
scalar a b = getSum . foldMap Sum $ (*) <$> a <*> b
