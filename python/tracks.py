def cyclic(li, index):
    c = 0
    for i in range(index, len(li)):
        if li[c] != li[i]:
            return False
        c = c + 1
    return True

def perm(accumulator, match_index, li):
    """
    This permutation function only generates the lexicographically smallest cyclic permutations of a tuple. Broadly, it does so by disallowing subtuples lexicographically smaller than the same length prefix of the generated tuple. match_index keeps track of the longest suffix of the generated tuple that is equal to the same length prefix.

    The function maintains the following invariants:
    1. accumulator[:match_index]==accumulator[-match_index:], and match_index is the biggest integer <len(accumulator) for which this is true
    2. accumulator[:a]<=accumulator[x:x+a] lexicographically for all x,a>=0,x+a<=len(accumulator)  

    When iterating through candidates for the next element in the permutation:
    1. If a candidate is smaller than accumulator[match_index], then accumulator[:match_index+1]>accumulator[-(match_index+1):], violating invariant 2. The search tree is pruned.
    2. If a candidate equals accumulator[match_index], then match_index is incremented.
    3. If a candidate is larger than accumulator[match_index], match_index is reset to 0. There is no suffix with length between 0 and match_index which equals a prefix, because say there were, with length mi2, then accumulator[:mi2]>accumulator[match_index+1-mi2:match_index+1], which contradicts invariant 2.

    Once there is nothing more left to permute, we need to check if the result is the lexicographically smallest cyclic permutation. It _will_ be as long as:
    1. There is no l, 0<l<len(accumulator), so accumulator[:l]>accumulator[-l:] lexicographically. But invariant 2 makes sure that is the case.
    2. There is no l, 0<l<len(accumulator), so accumulator[:l]==accumulator[-l:] and accumulator[l:]>accumulator[:-l]. This last condition is what cyclic() checks for l=match_index. In practise, < cannot happen because of invariant 2, so we just need to check equality. Equality means the permutation has rotational symmetry, which is where the function gets its name. Remaining is to prove if/why no check is needed for any l with 0<l<match_index.
    """
    if li == ():
        if match_index == 0 or cyclic(accumulator, match_index):
            yield accumulator
    else:
        for i, elem in enumerate(li):
            if elem not in li[:i] and elem >= accumulator[match_index]:
                for p in perm(accumulator + (elem,), match_index + 1 if elem == accumulator[match_index] else 0, li[:i] + li[i + 1:]):
                    yield p

def lex_min_rot_perm(li):
    for i, elem in enumerate(li):
        if elem not in li[:i]:
            for p in perm((elem,),0,li[:i] + li[i + 1:]):
                yield p

class Translation:
    """
    Translation(a,b,c,d) represents the vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3)

    The 30n degree rotations of the translations of the 3 kinds of pieces 
    Straight (2,0), (sqrt3,1),(1,sqrt3) etc,
    Left (2, 4-2sqrt3), (-2+2sqrt3, -2+2sqrt3), (4-2sqrt3, 2) etc,
    Right (2, -4+2sqrt3), (2, 4-2sqrt3), (-2+2sqrt3, -2+2sqrt3) etc
    can all be represented on the form (e+fsqrt3,g+hsqrt3), e,f,g,h integral.
    However, that is not the case for rotations of track pieces of this form in general,
    since the rotiation matrix of 30 deg: ((sqrt3/2, -1/2),(1/2, sqrt3/2)) applied to the
    expression above gives the rotated translation vector ((3f-g)/2 + (e-h)sqrt3/2, (e+3h)/2 + (f+g)sqrt3/2).
    However, from this last expression, it can be seen that if f and g have the same parity and e and h have
    the same parity (which is the case for the above representation of the track pieces), 
    the result of the rotation will be in the desired form since all of 3f-g, e-h, e+3h and f+g will be even. 
    Furthermore, note that the result of the rotation itself also satisfies the parity
    constraint, since (3f-g)/2 + (f+g)/2 = 2f, an even number, and likewise (e-h)/2 + (e+3h)/2 = e + h,
    also an even number by the original assumption.
    Of course, we could represent a translation by integers e,f,g,h above and use integer division for /2,
    assuming that the input satisfies the parity constraint, but that would be an un-robust format.
    A better, although much more confusing, solution is to have a representation which enforces the parity
    constraint. 
    There are multiple ways to represent 2 integers of same parity, one is to use one integer as their (integer)
    mean, and one as their difference from the mean (positive for one, negative for the other). This representation
    has the additional benefit that the rotation calculations are very simple (see below). 
    
    mnemonics are:
    xyia: (x)'s integral coefficient and (y)'s (i)rrational coefficient's (a)verage
    xyiv: (x)'s integral coefficient and (y)'s (i)rrational coefficient's (v)ariance
    xiya: (x)'s (i)rrational coefficient and (y)'s integral coefficient's (a)verage
    xiyv: (x)'s (i)rrational coefficient and (y)'s integral coefficient's (v)ariance

    """

    def __init__(self, xyia, xyiv, xiya, xiyv):
        self.xyia = xyia
        self.xyiv = xyiv
        self.xiya = xiya
        self.xiyv = xiyv

    def __add__(self, other):
        return Translation(self.xyia + other.xyia, self.xyiv + other.xyiv, self.xiya + other.xiya, self.xiyv + other.xiyv)

    def rotate(self, rotation):
        """ rotations are multiples of 30deg, counterclockwise. """
        if rotation == 0:
            return self
        elif rotation == 1:
            return Translation(self.xiya - self.xiyv, -self.xiyv, self.xyia, self.xyia - self.xyiv)
        elif rotation == -1:
            return Translation(self.xiya, self.xiya - self.xiyv, self.xyia - self.xyiv, -self.xyiv)
        else:
            print("cannot rotate by " + rotation)
            exit()


class Track:
    def __init__(self, translation, rotation):
        self.translation = translation
        self.rotation = rotation

    def __add__(self, other):
        return Track(self.translation + other.translation.rotate(self.rotation), self.rotation + other.rotation)


def get_piece(num):
    if num == 1:
        return Track(Translation(0, 2, 2, 2), 1) # (2,4-2sqrt3), left curve
    elif num == 2:
        return Track(Translation(2, 0, -2, -2), -1) # (2,2sqrt3-4), right curve
    elif num == 3:
        return Track(Translation(1, 1, 0, 0), 0) # (2,0), straight piece
    else:
        print("no track by number " + num)
        exit()

pieces = (1,) * 15 + (2,) * 3 + (3,) * 5
perms = lex_min_rot_perm(pieces)

for permutation in perms:
    accumulate = Track(Translation(0, 0, 0, 0), 0)
    for piece_num in reversed(permutation):
        accumulate = get_piece(piece_num) + accumulate
    tr = accumulate.translation
    if tr.xyia == 0 and tr.xyiv == 0 and tr.xiya == 0 and tr.xiyv == 0 and accumulate.rotation % 12 == 0:
        print(permutation)
