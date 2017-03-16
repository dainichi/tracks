import sys

L = 0
R = 1
S = 2
MAX = 3
track = [0] * 23
bag = [15 - 1, 3, 5]

def search(index, match, a, b, c, d):
    """
Searches for lexicographically smallest cyclic tracks using pieces in bag. Prunes when we are back at the origin.
index: current piece number
match: biggest number <index for which track[:match]==track[index-match:index]
a, b, c and d describe the translation, same representation as in tracks.py
Rotation/translation and outputting done inline
    """
    if a == 0 and b == 0 and c == 0 and d == 0:
        if index % (index - match) == 0 and bag[L] == bag[R]:
            for piece in track[:index]:
		sys.stdout.write({L:"L",R:"R",S:"S"}[piece])
            print("")
    else:
        for piece in range(track[match],MAX):
            if bag[piece] > 0:
                bag[piece] = bag[piece] - 1
                track[index] = piece
		if piece == L:
                    search(index + 1, match + 1 if track[match] == piece else 0, c - d, 2 - d, a + 2, a - b + 2)
                elif piece == R:
                    search(index + 1, match + 1 if track[match] == piece else 0, c + 2, c - d, a - b - 2, -b - 2)
                else:
                    search(index + 1, match + 1 if track[match] == piece else 0, a + 1, b + 1, c, d)
                bag[piece] = bag[piece] + 1

"""track is prepopulated with one L"""
search(1,0,0,2,2,2)
