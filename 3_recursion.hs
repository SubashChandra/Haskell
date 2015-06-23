factorial :: (Integral x)=> x->x
factorial 0 = 1
factorial n = n*factorial (n-1)


replicate':: (Num i, Ord i)=>i->a->[a]
replicate' n x
	| n<=0 = []
	| otherwise = x:replicate' (n-1) x


take':: (Num x, Ord x)=>x->[a]->[a]
take' n _
	| n<=0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs


reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

