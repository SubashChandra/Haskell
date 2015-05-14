lucky::(Integral a)=> a -> String
lucky 7 = "Lucky no seven"
lucky x = "out of luck"


sayMe:: (Integral a)=> a->String
sayMe 1="one"
sayMe 2="two"
sayMe 3="three"
sayMe 4="four"
sayMe 5="five"
sayMe x="not between 1 and 5"


charName:: Char -> String
charName 'a' = "apple"
charName 'b' = "banana"


addVectors::(Num a) => (a,a)->(a,a)->(a,a)
-- addVectors a b = (fst a +fst b, snd a+snd b)  can't handle triplets
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)
		

