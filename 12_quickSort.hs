quicksort:: (Ord a) => [a]-> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallestList = quicksort [a|a<-xs, a<=x]
	    largestList = quicksort [a|a<-xs, a>x]
	    in smallestList++[x]++largestList

quicksort1::(Ord a)=>[a]->[a]
quicksort1 [] = []
quicksort1 (x:xs) =
	let smallestList = quicksort1 (filter (<=x) xs)
	    largestList = quicksort1 (filter (>x) xs)
	    in smallestList++[x]++largestList
