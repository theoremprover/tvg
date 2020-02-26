module Mergesort where

-- (x:ls) = x first element, ls is rest list
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort left) (mergesort right)
	where
	halflen = div (length l) 2
	left  = take halflen l
	right = drop halflen l

	merge [] l = l
	merge l [] = l
	merge (x1:r1) (x2:r2) | x1 > x2 = x2 : merge (x1:r1) r2
	merge (x1:r1) (x2:r2)           = x1 : merge r1      (x2:r2)
