head'::[a]->a
head' [] = error "cant call head on an empty list"
head' (x:_) = x


-- some more on pattern matching 
tell::(Show x)=> [x]-> String
tell [] = "empty list"
tell (x:[]) = "lish has one element: "++show x
tell (x:y:[]) = "list has two elements: "++show x++ " and "++show y
tell (x:y:_) = "list is long and has more than 2 elements"
