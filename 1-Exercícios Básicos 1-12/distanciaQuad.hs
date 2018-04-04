distanciaQuad :: integer-> integer-> integer-> integer-> integer
distanciaQuad x y cx cy = (cx - x)^2 + (cy - y)^2

estaDentro :: integer-> integer-> integer-> integer-> integer-> Bool
estaDentro x y cx cy r = (distanciaQuad x y cx cy) >= r^2

main = do 
	print (distanciaQuad 1 1 4 4)
	print (estaDentro 1 1 4 4 5)