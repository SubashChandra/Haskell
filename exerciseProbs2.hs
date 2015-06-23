-- tasks given



map':: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs


filter':: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' p (x:xs) 
	|p x = x:filter' p xs
	|otherwise = filter' p xs


sumEven::(Num a)=>(a->Bool)->[a]->a
sumEven _ [] = 0
sumEven f (x:xs)
	| f x  = x+recValue
	|otherwise = recValue
	where recValue = sumEven f xs


sumEven'::(Integral a)=>[a]->a
sumEven' xs = foldl (+) 0 (filter even xs)

reverse'::[a]->[a]
reverse' = foldl (\acc x -> x:acc) []


removeSubLists::Int->[[a]]->[[a]]
removeSubLists _ [] = []
removeSubLists 0 _ = []
removeSubLists n (x:xs)
		| (length x) > n = x:removeSubLists n xs
		| otherwise = removeSubLists n xs

removeSubLists'::Int->[[a]]->[[a]]
removeSubLists' n xs = foldl (\acc x -> if length x<n then acc else acc++[x] )[] xs


removeSubLists''::Int->[[a]]->[[a]]
removeSubLists'' n xs = filter (\x->length x>=n) xs

identicalPairs::(Eq a)=>[(a,a)]->[(a,a)]
identicalPairs [] = []
identicalPairs ((x1,x2):xs)
		| x1== x2 = (x1,x2):identicalPairs xs
		| otherwise = identicalPairs xs

identicalPairs'::(Eq a)=>[(a,a)]->Bool
identicalPairs' xs = foldl (\acc (x1,x2) -> if x1 == x2 then acc else False) True xs

identicalPairs''::(Eq a)=>[(a,a)]->Bool
identicalPairs'' xs = all (\x -> fst x == snd x) xs


union'::(Eq a)=>[a]->[a]->[a]
union' [] [] = []
union' [] xs = xs
union' xs [] = xs
union' as bs = foldl (\xs y -> if elem y xs then xs else xs++ [y]) as bs


intersec'::(Eq a)=>[a]->[a]->[a]
intersec' _ [] = []
intersec' [] _ = []
intersec' as bs =
	let ns = [a|a<-as, elem a bs]
	in [b|b<-bs, elem b ns]




diff'::(Eq a)=>[a]->[a]->[a]
diff' [] _ = []
diff' as [] = as
diff' as bs = foldl (\xs y -> if elem y bs then xs else xs++ [y]) [] as
	

foldl'::(b->a->b)->b->[a]->b
foldl' f y [] = y
foldl' f y (x:xs)= foldl' f (f y x) xs 
	

foldr'::(a->b->b)->b->[a]->b
foldr' f y [] = y
foldr' f y (x:xs) =  f x (foldr' f y xs)


