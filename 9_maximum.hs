maximum'::(Ord a)=>[a]->a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xr)
	| x> maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xr
