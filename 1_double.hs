doubleMe x = x+x
doubleUs x y = doubleMe x+ doubleMe 

doubleSmallNumber x = if x>100
			then x
			else x*2
--here name is a function which doesn't take any parameters
name = "apple is not a name"

boomBangs xs = [if x<10 then "BOOM!" else "BANG!!" | x<-xs, odd x]

length' li = sum [1 | _<-li]

removeNonUppercase st = [c| c<-st, c `elem` ['A'..'Z']]

addThree x y z= x+y+z

circumference :: Float->Float
circumference r = 2*pi*r

circumference1 :: Double->Double
circumference1 r = 2*pi*r
