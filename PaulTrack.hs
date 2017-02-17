module PaulTrack where
import Tra
import Vec

data PaulTrack = L | R | S deriving (Show, Eq, Ord)

cords S = TRTrack (Vec 1 0)(UVec 1 0)
cords L = TRTrack (Vec 1 (2-sqrt(3))) (UVec (sqrt(3)/2) (1/2))
cords R = TRTrack (Vec 1 (sqrt(3)-2)) (UVec (sqrt(3)/2) (-1/2))

closed (TRTrack (Vec a b) (UVec c d)) =
    abs a < 0.01 && abs b < 0.01 && abs(c-1) < 0.01 && abs d < 0.01
