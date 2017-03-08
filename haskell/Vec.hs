{-# LANGUAGE MultiParamTypeClasses #-}
module Vec where
import Tra

data Vec a = Vec a a deriving (Show)
data UVec a = UVec a a deriving (Show)

instance Num a => Monoid (Vec a) where
    Vec a b `mappend` Vec c d = Vec (a+c) (b+d)
    mempty = Vec 0 0

instance Num a => Monoid (UVec a) where
    UVec m1 m2 `mappend` UVec n1 n2 = UVec(m1*n1-m2*n2)(m2*n1+m1*n2)
    mempty = UVec 1 0

instance Num a => RotPos (UVec a) (Vec a) where
    UVec m1 m2 <**> Vec v1 v2 = Vec(m1*v1 - m2*v2)(m2*v1 + m1*v2)
