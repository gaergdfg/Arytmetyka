let abs (x: float) = 
	if x > 0. then x
	else x *. (-1.)

let rec min a b c d =
	if a < b && a < c && a < d then a
	else min b c d infinity

let rec max a b c d =
	if a > b && a > c && a > d then a
	else max b c d neg_infinity