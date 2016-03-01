class T:
	def __init__(self, tid, l=None, r=None):
		self.l = l
		self.r = r
		self.id = tid

def euler_tour_1(tree):
	""" 
	Euler tour; Every vertex appear when we visit it
	up to it
	"""
	if tree is None:
		return
	print(tree.id, end=' ')
	euler_tour_1(tree.l)
	if not tree.l is None:
		print(tree.id, end=' ')
	euler_tour_1(tree.r)
	if not tree.r is None:
		print(tree.id, end=' ')

def euler_tour_2(tree):
	""" 
	Euler tour; Every vertex appear only before and after it's subtree
	"""
	if tree is None:
		return
	print(tree.id, end=' ')
	euler_tour_2(tree.l)
	euler_tour_2(tree.r)
	print(tree.id, end=' ')

def euler_tour_3(tree):
	""" 
	Euler tour; edges tour
	"""
	if not tree.l is None:
		print('[', tree.id, ' ', tree.l.id, ']', end=' ')
		euler_tour_3(tree.l)
		print('[', tree.l.id, ' ', tree.id, ']', end=' ')
	if not tree.r is None:
		print('[', tree.id, ' ', tree.r.id, ']', end=' ')
		euler_tour_3(tree.r)
		print('[', tree.r.id, ' ', tree.id, ']', end=' ')


if __name__ == "__main__":
	tree = T(1, 
			   T(2), 
			   T(3, 
			   		T(4), 
			   		T(5)))
	euler_tour_1(tree)
	print("")
	euler_tour_2(tree)
	print("")
	euler_tour_3(tree)