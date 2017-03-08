def eqTup(acc,nym) :
	return nym == () or (acc[0] == nym[0] and eqTup(acc[1:],nym[1:]))

def perm(acc, nym, l) :
	if l == () :
		if eqTup(acc,nym) :
			yield acc
	else :
		for i in range(0,len(l)) :
			elem = l[i]
			if elem not in l[:i] and not (len(nym)>0 and elem<nym[0]) :				
				if len(nym)>0 and elem == nym[0] :
					nnym = nym[1:]
				else:
					nnym = acc[:]
				for p in perm(acc + (elem,),nnym+(elem,),l[:i]+l[i+1:]) :
					yield p

#this strange representation seems necessary to represent all coordinates with integers.
#Translation(a,b,c,d) represents the vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3) 
class Translation :
	def __init__(self, xyia, xyiv, xiya, xiyv) :
		self.xyia = xyia
		self.xyiv = xyiv
		self.xiya = xiya
		self.xiyv = xiyv

	def __add__(self, other) :
		return Translation(self.xyia+other.xyia,self.xyiv+other.xyiv,self.xiya+other.xiya,self.xiyv+other.xiyv)

#rotations are multiples of 30deg, counterclockwise.
	def rotate(self, rotation) :
		if rotation == 0 :
			return self
		elif rotation == 1 :
			return Translation(self.xiya-self.xiyv, -self.xiyv, self.xyia, self.xyia-self.xyiv)
		elif rotation == -1 :
			return Translation(self.xiya, self.xiya-self.xiyv, self.xyia-self.xyiv, -self.xyiv)
		else :
			print("cannot rotate by " + rotation)
			exit()

class Track :
	def __init__(self, translation, rotation) :
		self.translation = translation
		self.rotation = rotation

	def __add__(self, other) :
		return Track(self.translation + other.translation.rotate(self.rotation), self.rotation + other.rotation)

def getPiece(num) :
	if num == 1 :
		return Track(Translation(0,2,2,2),1) #(2,4-2sqrt3), left curve
	elif num == 2 :
		return Track(Translation(2,0,-2,-2),-1) #(2,2sqrt3-4), right curve
	elif num == 3 :
		return Track(Translation(1,1,0,0),0) #(2,0), straight piece
	else :
		print("no track by number " + num)
		exit()

perms = perm((),(),(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,3,3))
for p in perms :
	acc = Track(Translation(0,0,0,0),0)
	for pieceNum in reversed(p) :
		acc = getPiece(pieceNum) + acc
	tr = acc.translation
	if tr.xyia == 0 and tr.xyiv == 0 and tr.xiya == 0 and tr.xiyv == 0 and acc.rotation % 12 == 0 :
		print(p)
