maximum'::(Ord a)=>[a]->a

maximum' [] = error "empty list"
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)
