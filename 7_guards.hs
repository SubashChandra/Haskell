bmiTell :: (RealFloat a)=>a->a->String

bmiTell w h
	| w/h^2 <=18.5 = "Underweight"
	| w/h^2 <=25.0 = "normal"
	| w/h^2 <=30.0 = "over weight"
	| otherwise = "whale"

myCompare :: (Ord a) => a->a->Ordering
a `myCompare` b
	| a>b = GT
	| a==b = EQ
	| otherwise = LT



