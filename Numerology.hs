{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Numerology where

import Data.Foldable (fold)

data N = Z | S N

data Dict c where
  Dict :: c => Dict c

type family (+) (a :: N) (b :: N) :: N where
  'S n + m = 'S (n + m)
  'Z + m = m


class Commutative m n where
  commutative :: Dict ((n + m) ~ (m + n))

class ZeroIsRightIdentity n where
  zeroIsRightIdentity :: Dict ((n + 'Z) ~ n)

instance ZeroIsRightIdentity 'Z where
  zeroIsRightIdentity = Dict

instance ZeroIsRightIdentity n => ZeroIsRightIdentity ('S n) where
  zeroIsRightIdentity = case zeroIsRightIdentity @n of
                          Dict -> Dict

instance ZeroIsRightIdentity n => Commutative 'Z n where
  commutative = case zeroIsRightIdentity @n of
                  Dict -> Dict

class OneOnEitherSide n m where
  oneOnEitherSide :: Dict ('S (n + m) ~ (n + 'S m))

instance OneOnEitherSide 'Z m where
  oneOnEitherSide = Dict

instance OneOnEitherSide n m => OneOnEitherSide ('S n) m where
  oneOnEitherSide = case oneOnEitherSide @n @m of
                      Dict -> Dict

instance (OneOnEitherSide n m, Commutative m n) => Commutative ('S m) n where
  commutative = case (commutative @m @n, oneOnEitherSide @n @m) of
                  (Dict, Dict) -> Dict

class Associative a b c where
  associative :: Dict (((a + b) + c) ~ (a + (b + c)))

instance Associative 'Z 'Z c where
  associative = Dict

instance Associative a b c => Associative ('S a) b c where
  associative = case associative @a @b @c of
                  Dict -> Dict

instance (OneOnEitherSide a (b + c), OneOnEitherSide a b, Associative a b c) => Associative a ('S b) c where
  associative = case (associative @a @b @c, oneOnEitherSide @a @b, oneOnEitherSide @a @(b + c)) of
                  (Dict, Dict, Dict) -> Dict

type One = 'S 'Z
type Two = 'S One
type Three = 'S Two
type Four = 'S Three
type Five = 'S Four


test :: ()
test = fold
  [ a
  , b
  , c
  , d
  , e
  , f
  , g
  , h
  , j
  ]
a :: Associative (One + Five + Four + Two) (Two + Five + Three) Three => ()
a = ()
b :: Associative (Three + Four + Two + One) (Two + 'Z + One) One => ()
b = ()
c :: Associative Four Four Four => ()
c = ()
d :: Associative Three Three Three => ()
d = ()
e :: Commutative Three Three => ()
e = ()
f :: Commutative Three Four => ()
f = ()
g :: Commutative One Three => ()
g = ()
h :: Commutative Two Four => ()
h = ()
j :: SubbyInverse (Four + Four + Four + Four) (Five + Five) => ()
j = ()

type family (-) (a :: N) (b :: N) :: N where
  'S n - 'S m = n - m
  n - 'Z = n

class SubbyInverse a b where
  subbyInverse :: Dict (((a + b) - b) ~ a)

instance Commutative a 'Z => SubbyInverse a 'Z where
  subbyInverse = case commutative @a @('Z) of
                   Dict -> Dict

instance (OneOnEitherSide a b, SubbyInverse a b) => SubbyInverse a ('S b) where
  subbyInverse = case (oneOnEitherSide @a @b, subbyInverse @a @b) of
                   (Dict, Dict) -> Dict
