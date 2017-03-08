data Vec a = Vec a a deriving (Show)
data Mat a = Mat a a a a deriving (Show)
data Track a = Track (Vec a)(Mat a) deriving (Show)

odst = 1 / sqrt 2
s = Track (Vec 1 0) (Mat 1 0 0 1)
h = Track (Vec (1/2) 0) (Mat 1 0 0 1)
d = Track (Vec 2 0) (Mat 1 0 0 1)
l = Track (Vec odst (1 - odst))(Mat odst (-odst) odst odst)
r = Track (Vec odst (odst - 1))(Mat odst odst (-odst) odst)

Vec a b <+> Vec c d = Vec (a+c) (b+d)
Mat m11 m12 m21 m22 <**> Vec v1 v2 = Vec(m11*v1 + m12*v2)(m21*v1 + m22*v2)
Mat m11 m12 m21 m22 <***> Mat n11 n12 n21 n22 = Mat(m11*n11 + m12*n21)(m11*n12 + m12*n22)(m21*n11 + m22*n21)(m21*n12 + m22*n22)
 
Track v1 m1 <=> Track v2 m2 = Track(v1 <+> (m1 <**> v2))(m1 <***> m2)

mine = [s,d,h,l,l,l,l,h,s,l,l,r,l,s,l,l,r,r,r,r,r,l,r,r,r,l,s,h,h,d,l,l,h,l,r,l,l,h,l,r,l,s,l,l,l,l,r,l,l,l,l,l,l,l,r,s]

result = foldl1 (<=>) mine

{-
data Track = St Int | L | R deriving(Show)

data Dir = N | NW | W | SW | S | SE | E | NE deriving (Show)

uv N  = ((0,0),(1, 0))
uv NW = ((0,-1),(0,1))
uv W  = ((-1,0),(0,0))
uv SW = ((0,-1),(0,-1))
uv S  = ((0,0),(-1,0))
uv SE = ((0,1),(0,-1))
uv E  = ((1,0),(0,0))
uv NE = ((0,1),(0,1))

data Coor = C Int Int -- multiples of 1, multiples of 1/sqrt(2)
data Pos =  P Coor Coor
type Posdir = (Dir, Pos)

tDir N  L = NW
tDir NW L = W
tDir W  L = SW
tDir SW L = S
tDir S  L = SE
tDir SE L = E
tDir E  L = NE
tDir NE L = N
tDir N  R = NE
tDir NW R = N
tDir W  R = NW
tDir SW R = W
tDir S  R = SW
tDir SE R = S
tDir E  R = SE
tDir NE R = E
tDir d (St _) = d
-}

