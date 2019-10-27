(* Typ reprezentujący niedokładne wartości. *)
type wartosc = Range of (float * float) * (float * float)


(*---------------------------Utility---------------------------*)
(* sprawdza, czy poczatek lub koniec przedzialu to nan *)
let is_nan range = not (fst range >= snd range || fst range < snd range)

(* wartosc bezwzgledna float'a *)
let abs (x: float) = 
	if x > 0. then x
	else (-. x)

(* min czterech float'ow *)
let rec min a b c d =
	if is_nan (a, b) && is_nan (c, d) then nan
	else if is_nan (c, d) then min a b infinity infinity
	else if a < b && a < c && a < d then a
	else min b c d infinity

(* max czterech float'ow *)
let rec max a b c d =
	if is_nan (a, b) && is_nan (c, d) then nan
	else if is_nan (c, d) then max a b neg_infinity neg_infinity
	else if a > b && a > c && a > d then a
	else max b c d neg_infinity


(*---------------------------Constructors---------------------------*)
(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc a p = 
	let d = a *. (1. -. p)
	in Range((a -. d, a +. d), (nan, nan))

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do (a: float) (b: float) = Range((a, b), (nan, nan))

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna (a: float) = Range((a, a), (nan, nan))


(*---------------------------Queries---------------------------*)
(* in_wartosc range x = czy x nalezy do range *)
let in_wartosc (range: wartosc) (x: float) =
	let Range(a_left, a_right) = range
	in if (fst a_left <= x && x <= snd a_left) || (fst a_right <= x && x <= snd a_right) then true else false


(* min_wartosc range = najmniejsza możliwa wartość range,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.		*)
let min_wartosc (range: wartosc) =
	let Range((value, _), (_, _)) = range
	in value


(* max_wartosc range = największa możliwa wartość range,    *)
(* lub infinity jeśli brak górnego ograniczenia.    		*)
let max_wartosc (range: wartosc) =
	let Range(range_left, range_right) = range
	in if is_nan range_right then snd range_left else snd range_right


(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc (range: wartosc) =
	let Range(range_left, range_right) = range
	in if is_nan range_right then
		let mid = (fst range_left +. snd range_left) /. 2.
		in if (abs mid) <> infinity then mid else nan
	else
		nan


(*---------------------------Operations---------------------------*)
(*  {x + y:  x nalezy do a  i y nalezy do b}  *)
let plus (a: wartosc) (b: wartosc) = 
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan a_right, is_nan b_right) with
			(true, true)   ->
				(fst a_left +. fst b_left, 	snd a_left +. snd b_left),
				(nan, 						nan) |
			(true, false)  ->
				(neg_infinity, 				snd b_left +. snd a_left),
				(fst b_right +. fst a_left, infinity) |
			(false, true)  ->
				(neg_infinity, 				snd a_left +. snd b_left),
				(fst a_right +. fst b_left, infinity) |
			(false, false) ->
				(neg_infinity, 				infinity),
				(nan, 						nan)
		in if fst a_right >= snd a_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;

(*  {x - y:  x nalezy do a  i y nalezy do b}  *)
let minus (a: wartosc) (b: wartosc) =
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan a_right, is_nan b_right) with
			(true, true)   -> 
				(fst a_left -. snd b_left, 	snd a_left -. fst b_left),
				(nan, 						nan) |
			(true, false)  ->
				(neg_infinity, 				snd a_left -. fst b_left),
				(fst a_left -. snd b_left, 	infinity) |
			(false, true)  ->
				(neg_infinity, 				snd a_left -. fst b_left),
				(fst a_right -. snd b_left, infinity) |
			(false, false) ->
				(neg_infinity, 				infinity),
				(nan, 						nan)
		in if fst a_right >= snd a_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;

(*  [a,b] * [c,d]  *)
let mult_single_single a b =
	let (q, w) = a
	and (e, r) = b
	in (min (q *. e) (q *. r) (w *. e) (w *. r),
		max (q *. e) (q *. r) (w *. e) (w *. r))
;;
(*  [a, b] * {(-inf, c] u [d, _inf)}  *)
let mult_single_double a b =
	let (b_left, b_right) = b
	in
		let res_left = mult_single_single a b_left
		and res_right = mult_single_single a b_right
		in
			if fst res_left = neg_infinity && fst res_right = neg_infinity then
				(neg_infinity, max (snd res_left) (snd res_right) neg_infinity neg_infinity), (nan, nan)
			else if fst res_left = neg_infinity && fst res_right <> neg_infinity then
				(res_left, res_right)
			else (res_right, res_left)
;;
(*  {x * y:  x nalezy do a  i  y nalezy do b}  *)
let razy (a: wartosc) (b: wartosc) =
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan a_right, is_nan b_right) with
			(*  [a, b]*[c, d]  *)
			(true, true)   -> (mult_single_single a_left b_left), (nan, nan) |

			(*  [a, b] * ((-inf, c] u [d, +inf)) = ([a, b] * (-inf, c]) u ([a, b] * [d, +inf))  *)
			(true, false)  -> if a_left = (0., 0.) then (0., 0.), (nan, nan) else mult_single_double a_left (b_left, b_right) |

			(*  ((-inf, a] u [b, +inf)) * [c, d] = ((-inf, a] * [c, d]) u ([b, +inf) * [c, d])  *)
			(false, true)  -> if b_left = (0., 0.) then (0., 0.), (nan, nan) else mult_single_double b_left (a_left, a_right) |

			(*  ((-inf, a] u [b, +inf)) * ((-inf, c] u [d, +inf)) = {(-inf, a] * ((-inf, c] u [d, +inf))} u {[b, +inf) * ((-inf, c] u [d, +inf))}  *)
			(false, false) ->
				if snd a_left > 0. || fst a_right < 0. || snd b_left > 0. || fst b_right < 0. then
					(neg_infinity, infinity), (nan, nan)
				else
					let sub_res_left = mult_single_double a_left (b_left, b_right)
					and sub_res_right = mult_single_double a_right (b_left, b_right)
					in
						(neg_infinity,		max (snd (fst sub_res_left)) (snd (fst sub_res_right)) neg_infinity neg_infinity),
						(max (fst (snd sub_res_left)) (fst (snd sub_res_right)) infinity infinity,		infinity)
		in if fst a_right >= snd a_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;


let podzielic (a: wartosc) (b: wartosc) = Range((1., 1.), (nan, nan))
;;
