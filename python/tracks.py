def eq_tup(acc, nym):
    return nym == () or (acc[0] == nym[0] and eq_tup(acc[1:], nym[1:]))


def perm(acc, nym, li):
    if li == ():
        if eq_tup(acc, nym):
            yield acc
    else:
        for i in range(0, len(li)):
            elem = li[i]
            if elem not in li[:i] and not (len(nym) > 0 and elem < nym[0]):
                if len(nym) > 0 and elem == nym[0]:
                    nnym = nym[1:]
                else:
                    nnym = acc[:]
                for p in perm(acc + (elem,), nnym + (elem,), li[:i] + li[i + 1:]):
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
perms = perm((), (), pieces)

for permutation in perms:
    accumulate = Track(Translation(0, 0, 0, 0), 0)
    for piece_num in reversed(permutation):
        accumulate = get_piece(piece_num) + accumulate
    tr = accumulate.translation
    if tr.xyia == 0 and tr.xyiv == 0 and tr.xiya == 0 and tr.xiyv == 0 and accumulate.rotation % 12 == 0:
        print(permutation)
