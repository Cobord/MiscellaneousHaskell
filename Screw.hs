{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Screw
(
)
where

import Numeric.Algebra as Alg (LeftModule(..),RightModule(..),Additive(..),Multiplicative(..),Commutative(..),Group(..))
import Numeric.Algebra.Dual (Dual(..))
import Numeric.Algebra.Class (Semiring)


data Vec3 a = Vec3 { getX :: a, getY :: a, getZ :: a }
data Screw a = Screw {s :: Vec3 a,v :: Vec3 a}

instance (Additive a) => Additive (Vec3 a) where
    Vec3{getX=x1,getY=y1,getZ=z1} + Vec3{getX=x2,getY=y2,getZ=z2} = Vec3{getX=x1 Alg.+ x2,getY=y1 Alg.+ y2,getZ=z1 Alg.+ z2}

dotProduct :: (Additive a,Semiring a) => Vec3 a -> Vec3 a -> a
dotProduct Vec3{getX=x1,getY=y1,getZ=z1} Vec3{getX=x2,getY=y2,getZ=z2} = (x1 Alg.* x2) Alg.+ (y1 Alg.* y2) Alg.+ (z1 Alg.* z2)

crossProduct :: (Additive a,Semiring a,Group a) => Vec3 a -> Vec3 a -> Vec3 a
crossProduct Vec3{getX=x1,getY=y1,getZ=z1} Vec3{getX=x2,getY=y2,getZ=z2} = Vec3{getX=x3,getY=y3,getZ=z3} where
    x3 = y1 Alg.* z2 Alg.- z1 Alg.* y2
    y3 = z1 Alg.* x2 Alg.- x1 Alg.* z2
    z3 = x1 Alg.* y2 Alg.- y1 Alg.* x2

instance (Additive a) => Additive (Screw a) where
    screw1 + screw2 = Screw{s=(s screw1) Alg.+ (s screw2),v = (v screw1) Alg.+ (v screw2)}

instance (Semiring a) => LeftModule a (Vec3 a) where
    a .* Vec3{getX=x,getY=y,getZ=z} = Vec3{getX=a Alg.* x,getY=a Alg.* y,getZ=a Alg.* z}

instance (Additive a,Semiring a,Commutative a,Group a) => LeftModule (Dual a) (Screw a) where
    (Dual a b) .* screw = Screw{s=a .* (s screw),v= (a .* (v screw)) Alg.+ (b .* (s screw))}

dotProductScrew :: (Additive a,Semiring a) => Screw a -> Screw a -> Dual a
dotProductScrew screw1 screw2 = 
    let t1 = dotProduct (s screw1) (s screw2)
        t2 = dotProduct (s screw1) (v screw2)
        t3 = dotProduct (v screw1) (s screw2)
    in Dual t1 (t2 Alg.+ t2)

crossProductScrew :: (Additive a,Semiring a,Group a) => Screw a -> Screw a -> Screw a
crossProductScrew screw1 screw2 = 
    let t1 = crossProduct (s screw1) (s screw2)
        t2 = crossProduct (s screw1) (v screw2)
        t3 = crossProduct (v screw1) (s screw2)
    in Screw{s=t1,v=(t2 Alg.+ t2)}