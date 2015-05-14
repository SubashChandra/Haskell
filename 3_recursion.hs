factorial :: (Integral x)=> x->x
factorial 0 = 1
factorial n = n*factorial (n-1)
