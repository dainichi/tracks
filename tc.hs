{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import GHC.Real

data Vec a = Vec a a deriving (Show)
data Mat a = Mat a a a a deriving (Show)

instance Num a => Monoid (Vec a) where
    Vec a b `mappend` Vec c d = Vec (a+c) (b+d)
    mempty = Vec 0 0

instance Num a => Monoid (Mat a) where
    Mat m11 m12 m21 m22 `mappend` Mat n11 n12 n21 n22 = Mat(m11*n11+m12*n21)(m11*n12+m12*n22)(m21*n11+m22*n21)(m21*n12+m22*n22)
    mempty = Mat 1 0 0 1

class RotPos r p where
    (<**>) :: r -> p -> p

instance Num a => RotPos (Mat a) (Vec a) where
    Mat m11 m12 m21 m22 <**> Vec v1 v2 = Vec(m11*v1 + m12*v2)(m21*v1 + m22*v2)

data Track a = Track (Vec a)(Mat a) deriving (Show)

instance Num a => Monoid (Track a) where
    Track v1 m1 `mappend` Track v2 m2 = Track(v1 `mappend` (m1 <**> v2))(m1 `mappend` m2)
    mempty = Track mempty mempty

class Monoid t => Piece t where
    s::t
    h::t
    d::t
    l::t
    r::t

odst = 1 / sqrt 2

instance Piece (Track Double) where
    s = Track (Vec 1 0) mempty
    h = Track (Vec (1/2) 0) mempty
    d = Track (Vec 2 0) mempty
    l = Track (Vec odst (1 - odst))(Mat odst (-odst) odst odst)
    r = Track (Vec odst (odst - 1))(Mat odst odst (-odst) odst)

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

instance Piece (Track RI) where
    s = Track (Vec 1 0) mempty
    h = Track (Vec (RI (1%2) 0) 0) mempty
    d = Track (Vec 2 0) mempty
    l = Track (Vec iu (1 - iu))(Mat iu (-iu) iu iu)
    r = Track (Vec iu (iu - 1))(Mat iu iu (-iu) iu)

data Angle = Angle Int deriving (Show)

instance Monoid Angle where
    Angle a `mappend` Angle b = Angle (a+b)
    mempty = Angle 0

instance RotPos Angle (Vec RI) where
    Angle a <**> v = 
        (case a `mod` 8 of 0 -> mempty
                           1 -> Mat iu (-iu) iu iu
                           2 -> Mat 0 (-1) 1 0
                           3 -> Mat (-iu)(-iu)iu(-iu)
                           4 -> Mat (-1) 0 0 (-1)
                           5 -> Mat (-iu)iu(-iu)(-iu)
                           6 -> Mat 0 1 (-1) 0
                           7 -> Mat iu iu (-iu) iu) <**> v

data ATrack = ATrack (Vec RI) Angle deriving (Show)

instance Monoid ATrack where
    ATrack v1 m1 `mappend` ATrack v2 m2 = ATrack(v1 `mappend` (m1 <**> v2))(m1 `mappend` m2)
    mempty = ATrack mempty mempty

instance Piece ATrack where
    s = ATrack (Vec 1 0) mempty
    h = ATrack (Vec (RI (1%2) 0) 0) mempty
    d = ATrack (Vec 2 0) mempty
    l = ATrack (Vec iu (1 - iu))(Angle 1)
    r = ATrack (Vec iu (iu - 1))(Angle (-1))

            


mine::Piece t => [t]
mine = [s,d,h,l,l,l,l,h,s,l,l,r,l,s,l,l,r,r,r,r,r,l,r,r,r,l,s,h,h,d,l,l,h,l,r,l,l,h,l,r,l,s,l,l,l,l,r,l,l,l,l,l,l,l,r,s]

doubleResult = mconcat (mine::[Track Double])
riResult = mconcat (mine::[Track RI])
atResult = mconcat (mine::[ATrack])

