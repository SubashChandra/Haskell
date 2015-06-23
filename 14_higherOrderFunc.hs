--function that takes a function as parameter and applies it twice
applyTwice:: (a->a)->a->a
applyTwice f x = f(f x)


zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

--functions with partially supplied parameters returns functions called curried functions
map'::(a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x: map' f xs


--sum of even numbers
sumEven :: (Num a)=>(a->Bool)->[a]->a
sumEven _ [] =0
sumEven f (x:xs) 
	| f x = x+sumEven f xs
	| otherwise = sumEven f xs


--takeWhile 
-- sum of all odd squares that are less than 10000
--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

--chain 
chain::(Integral a)=>a->[a]
chain 1 = [1]
chain n
	|even n = n:chain (n `div` 2)
	|odd n = n:chain (3*n+1)


--number of chains for all starting from 1 to 100. how many are of length greater than 15
numLongChains::Int
numLongChains = length (filter isLong (map chain [1..100]))
		where isLong xs = length xs >16

sumEven' :: (Eq a, Num a,Integral a)=>[a]->a
sumEven' xs = foldl (\acc x -> if x `mod` 2==0 then acc+x else acc) 0 xs
