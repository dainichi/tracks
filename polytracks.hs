{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import GHC.Real

data Vec a = Vec a a deriving (Show)
data UVec a = UVec a a deriving (Show)

instance Num a => Monoid (Vec a) where
    Vec a b `mappend` Vec c d = Vec (a+c) (b+d)
    mempty = Vec 0 0

instance Num a => Monoid (UVec a) where
    UVec m1 m2 `mappend` UVec n1 n2 = UVec(m1*n1-m2*n2)(m2*n1+m1*n2)
    mempty = UVec 1 0

class RotPos r p where
    (<**>) :: r -> p -> p

instance Num a => RotPos (UVec a) (Vec a) where
    UVec m1 m2 <**> Vec v1 v2 = Vec(m1*v1 - m2*v2)(m2*v1 + m1*v2)

data TRTrack t r = TRTrack t r deriving (Show)

instance (Monoid t, Monoid r, RotPos r t) => Monoid (TRTrack t r) where
    TRTrack t1 r1 `mappend` TRTrack t2 r2 = TRTrack(t1 `mappend` (r1 <**> t2))(r1 `mappend` r2)
    mempty = TRTrack mempty mempty

class Monoid t => Piece t where
    s::t
    h::t
    d::t
    l::t
    r::t

odst = 1 / sqrt 2

instance Piece (TRTrack (Vec Double)(UVec Double)) where
    s = TRTrack (Vec 1 0) mempty
    h = TRTrack (Vec (1/2) 0) mempty
    d = TRTrack (Vec 2 0) mempty
    l = TRTrack (Vec odst (1 - odst))(UVec odst odst)
    r = TRTrack (Vec odst (odst - 1))(UVec odst (-odst))

data RI = RI Rational Rational -- deriving (Show)

instance Show RI where
    show (RI (rn :% rd)(ino :% id)) = (if rn == 0 
                                       then (if ino == 0 then "0" else "") 
                                       else show rn ++ (if rd == 1 
                                                        then "" 
                                                        else "/" ++ show rd)) ++
                                    if ino == 0 then "" else ((if ino >=0 then "+" else "") ++
                                    show ino ++ "/" ++ (if id == 1 then "" else show id) ++ "sqrt2")

instance Num RI where
    RI r1 i1 + RI r2 i2 = RI (r1+r2)(i1+i2)
    RI r1 i1 * RI r2 i2 = RI (r1*r2+i1*i2/2)(r1*i2+r2*i1)
    RI r1 i1 - RI r2 i2 = RI (r1-r2)(i1-i2)
    fromInteger i = RI (i%1) 0

iu = RI 0 1

instance Piece (TRTrack (Vec RI) (UVec RI)) where
    s = TRTrack (Vec 1 0) mempty
    h = TRTrack (Vec (RI (1%2) 0) 0) mempty
    d = TRTrack (Vec 2 0) mempty
    l = TRTrack (Vec iu (1 - iu))(UVec iu iu)
    r = TRTrack (Vec iu (iu - 1))(UVec iu (-iu))

data Angle = Angle Int deriving (Show)

instance Monoid Angle where
    Angle a `mappend` Angle b = Angle (a+b)
    mempty = Angle 0

instance RotPos Angle (Vec RI) where
    Angle a <**> v = 
        (case a `mod` 8 of 0 -> mempty
                           1 -> UVec iu iu
                           2 -> UVec 0 1
                           3 -> UVec (-iu)iu
                           4 -> UVec (-1) 0
                           5 -> UVec (-iu)(-iu)
                           6 -> UVec 0 (-1)
                           7 -> UVec iu (-iu)) <**> v

data ATrack = ATrack (Vec RI) Angle deriving (Show)

instance Piece (TRTrack (Vec RI) Angle) where
    s = TRTrack (Vec 1 0) mempty
    h = TRTrack (Vec (RI (1%2) 0) 0) mempty
    d = TRTrack (Vec 2 0) mempty
    l = TRTrack (Vec iu (1 - iu))(Angle 1)
    r = TRTrack (Vec iu (iu - 1))(Angle (-1))

mine::Piece t => [t]
mine = [s,d,h,l,l,l,l,h,s,l,l,r,l,s,l,l,r,r,r,r,r,l,r,r,r,l,s,h,h,d,l,l,h,l,r,l,l,h,l,r,l,s,l,l,l,l,r,l,l,l,l,l,l,l,r,s]

doubleResult = mconcat (mine::[TRTrack (Vec Double)(UVec Double)])
riResult = mconcat (mine::[TRTrack (Vec RI)(UVec RI)])
atResult = mconcat (mine::[TRTrack (Vec RI) Angle])

