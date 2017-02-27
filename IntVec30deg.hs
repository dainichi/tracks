{-# LANGUAGE MultiParamTypeClasses #-}
module IntVec30deg where
import Tra

--this strange representation seems necessary to represent all cordinates with integers.
--IntTra30 a b c d represents the vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3) 
data IntTra30 = IntTra30 Int Int Int Int

instance Show IntTra30 where
    show (IntTra30 a b c d) = 
      let
        pr x y = if y==0
                 then show x
                 else (if x==0 then "" else show x) ++
                      (if x/=0 && y>0 then "+" else "") ++
                      (case y of 1 -> ""
                                 -1 -> "-"
                                 _ -> show y) ++ "sqrt3"
      in
        "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")=(" ++ 
                                pr (a+b) (c-d) ++ "," ++ pr (c+d) (a-b) ++ ")"

instance Monoid IntTra30 where
    IntTra30 a1 b1 c1 d1 `mappend` IntTra30 a2 b2 c2 d2 = IntTra30 (a1+a2)(b1+b2)(c1+c2)(d1+d2)
    mempty = IntTra30 0 0 0 0
        
newtype IntAng30 = IntAng30 Int deriving (Show)

instance Monoid IntAng30 where
    IntAng30 a `mappend` IntAng30 b = IntAng30 (a+b)
    mempty = IntAng30 0

instance RotPos IntAng30 IntTra30 where
    IntAng30 1 <**> IntTra30 a b c d = IntTra30 (c-d) (-d) a (a-b)
    IntAng30 0 <**> x = x
    IntAng30 (-1) <**> IntTra30 a b c d = IntTra30 c (c-d) (a-b) (-b)
