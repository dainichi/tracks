def cyclic(li, index):
    c = 0
    for i in range(index, len(li)):
        if li[c] != li[i]:
            return False
        c = c + 1
    return True

def perm(accumulator, match_index, li):
    if li == ():
        if match_index == 0 or cyclic(accumulator, match_index):
            yield accumulator
    else:
        for i in range(0, len(li)):
            elem = li[i]
            if elem not in li[:i] and elem >= accumulator[match_index]:
                for p in perm(accumulator + (elem,), match_index + 1 if elem == accumulator[match_index] else 0, li[:i] + li[i + 1:]):
                    yield p

def lex_min_rot_perm(li):
    for i in range(0, len(li)):
        elem = li[i]
        for p in perm((elem,),0,li[:i] + li[i + i:]):
            yield p

#this strange representation seems necessary to represent all coordinates with integers.
class Translation:
    """
    Translation(a,b,c,d) represents the vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3)
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
