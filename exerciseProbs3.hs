--tasks set 3

take'::Int ->[a]->[a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

takeWhile'::(a->Bool)->[a]->[a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
	| f x = x:takeWhile' f xs
	| otherwise = []

dropWhile'::(a->Bool)->[a]->[a]
dropWhile' _ [] = []
dropWhile' f all@(x:xs)
	| f x = dropWhile' f xs
	|otherwise = all


zip'::[a]->[b]->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


unzip'::[(a,b)]->([a],[b])
unzip' [] = ([],[])
unzip' ((x1,y1):xs) = (x1:fst list , y1:snd list )
	where list = unzip' xs


zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys 


splitAt'::(Int)->[a]->([a],[a])
splitAt' _ [] = ([],[])
splitAt' n xs = (take n xs, drop n xs)


span'::(a->Bool)->[a]->([a],[a])
span' _ [] = ([],[])
span' f all@(x:xs) 
	| f x = (x:fst ans, snd ans)
	| otherwise = ([],all)
	where ans = span' f xs

span''::(a->Bool)->[a]->([a],[a])
span'' _ [] = ([],[])
span'' f xs = (takeWhile f xs, dropWhile f xs)


listsSort::[[a]]->[[a]]
listsSort []= []
listsSort (x:xs) = listsSort [y|y<-xs, length y <= length x] ++ [x] ++ listsSort [y|y<-xs, length y> length x]


unique::(Eq a)=>[a]->[a]
unique [] = []
unique xs = foldr (\x acc -> if x `elem` acc 
		then acc 
		else x:acc) [] xs


group::(Eq a)=>[a]->[[a]]
group [] = [[]]
group xs = foldr (\x acc -> if x `elem` (head acc)|| length (head acc)==0 
		then  [x:(head acc)]++tail acc  
		else [x]:acc) [[]] xs



compress::(Eq a)=>[a]->[(a,Int)]
compress [] = []
compress xs = foldr (\x acc -> (head x,length x):acc) [] (group xs)
		


uncompress::[(a,Int)]->[a]
uncompress [] = []
uncompress xs = foldr (\(x,y) acc ->  (take y (repeat x))++acc) [] xs
		





