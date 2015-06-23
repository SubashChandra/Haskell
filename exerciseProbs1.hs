abs':: (Ord n, Num n)=> n->n
abs' n
	| n<0 = (-n)
	| otherwise = n


signum':: (Ord n, Num n) =>n->n
signum' n
	| n<0 = (-1)
	| n==0 = 0
	| otherwise = 1


head'::[n]->n
head' [] = error "empty list"
head' (x:_) = x


tail'::[n]->[n]
tail' [] = error "empty list"
tail' [x] = []
tail' (_:xs) = xs


take'::(Num i, Ord i)=> i->[a]->[a]
take' n _
	| n<=0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs


last'::[n]->n
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs


init'::[n]->[n]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x:init' xs


--sum of even numbers in a list
sumEven::(Ord n, Integral n)=>[n]->n
sumEven [] = 0
sumEven [x]
	| mod x 2==0 = x
	|otherwise = 0
sumEven (x:xs)
	| mod x 2 ==0 = x+sumEven xs
	|otherwise = sumEven xs


--list sorted or not
isSorted::(Ord a, Num a,Eq a)=>[a]->Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) 
	| x<=head xs = isSorted xs
	| otherwise = False


--list of tuples, return true if in each tuple all elements
eqTuples::(Eq a)=>[(a,a)]->Bool
eqTuples [] = error "empty list"
eqTuples [(a,b)] 
	| a==b = True
	| otherwise = False
eqTuples (x:xs)
	| fst x == snd x = eqTuples xs
	| otherwise = False


--implement replicate using repeat
replicate' ::Int->a->[a]
replicate' n x = take n (repeat x)




-- kth element in a list
elementK'::(Ord k,Num k)=>k->[a]->a
elementK' _ [] = error "index out of range"
elementK' k _
	| k<=0 = error "index out of range"
elementK' k (x:xs)
	| k==1 = x
	| otherwise = elementK' (k-1) xs




