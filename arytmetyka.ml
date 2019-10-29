(* Typ reprezentujący niedokładne wartości. *)
type wartosc = Range of (float * float) * (float * float)


(*---------------------------Utility---------------------------*)
(* sprawdza, czy poczatek lub koniec przedzialu to nan *)
let is_nan_range range = not (fst range >= snd range || fst range < snd range)

(*  sprawdza, czy liczba to nan  *)
let is_nan_number number = not (number = number)

(* wartosc bezwzgledna float'a *)
let abs (x: float) = 
	if x > 0. then x
	else (-. x)

(* min czterech float'ow lub nan, gdy wszystkie to nan *)
let rec min a b c d =
	if is_nan_number a && is_nan_number b && is_nan_number c && is_nan_number d then nan
	else
		let ignore_nan value = if value = value then value else infinity
		in if (ignore_nan a) <= (ignore_nan b) && (ignore_nan a) <= (ignore_nan c) && (ignore_nan a) <= (ignore_nan d) then (ignore_nan a)
		else min (ignore_nan b) (ignore_nan c) (ignore_nan d) infinity

(* max czterech float'ow lub nan, gdy wszystkie to nan *)
let rec max a b c d =
	if is_nan_number a && is_nan_number b && is_nan_number c && is_nan_number d then nan
	else
		let ignore_nan value = if value = value then value else neg_infinity
		in if (ignore_nan a) >= (ignore_nan b) && (ignore_nan a) >= (ignore_nan c) && (ignore_nan a) >= (ignore_nan d) then (ignore_nan a)
		else max (ignore_nan b) (ignore_nan c) (ignore_nan d) neg_infinity


(*---------------------------Constructors---------------------------*)
(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc a p = 
	let d = (abs a) *. (p /. 100.)
	in Range((a -. d, a +. d), (nan, nan))

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do (a: float) (b: float) = Range((a, b), (nan, nan))

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna (a: float) = Range((a, a), (nan, nan))

(*  pomocniczy konstruktor  *)
let nowy_przedzial a b c d = Range((a, b), (c, d))


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
	in if is_nan_range range_right then snd range_left else snd range_right


(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc (range: wartosc) =
	let Range(range_left, range_right) = range
	in if is_nan_range range_right then (fst range_left +. snd range_left) /. 2. else nan


(*---------------------------Operations---------------------------*)
(*  {x + y:  x nalezy do a  i y nalezy do b}  *)
(*  [a, b] + [c, d] = [a + b, c + d]  *)
(*  zwraca sume dwoch przedzialow wedlug tresci zadania  *)
let plus (a: wartosc) (b: wartosc) = 
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan_range a_right, is_nan_range b_right) with
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
		(*  laczy (-inf, b] i [c, +inf), jesli c >= b  *)
		in if fst res_right <= snd res_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;

(*  {x - y:  x nalezy do a  i y nalezy do b}  *)
(*  [a, b] - [c, d] = [a - d, b - c]  *)
(*  zwraca roznice przedzialow wedlug tresci zadania  *)
let minus (a: wartosc) (b: wartosc) =
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan_range a_right, is_nan_range b_right) with
			(true, true)   -> 
				(fst a_left -. snd b_left, 	snd a_left -. fst b_left),
				(nan, 						nan) |
			(true, false)  ->
				(neg_infinity, 				snd a_left -. fst b_right),
				(fst a_left -. snd b_left, 	infinity) |
			(false, true)  ->
				(neg_infinity, 				snd a_left -. fst b_left),
				(fst a_right -. snd b_left, infinity) |
			(false, false) ->
				(neg_infinity, 				infinity),
				(nan, 						nan)
		(*  laczy (-inf, b] i [c, +inf), jesli c >= b  *)
		in 
			if fst res_right <= snd res_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;

(*  [a,b] * [c,d] = [min(ac, ad, bc, bd), max(ac, ad, bc, bd)]  *)
(*  zwraca wynik mnozenia dwoch przedzialow, z ktorych zadny nie jest rozlaczny  *)
let mult_single_single a b =
	if is_nan_range a || is_nan_range b then (nan, nan)
	else
		let (q, w) = a
		and (e, r) = b
		in 
			if a = (0., 0.) || b = (0., 0.) then (0., 0.)
			else
				(min (q *. e) (q *. r) (w *. e) (w *. r),
				max (q *. e) (q *. r) (w *. e) (w *. r))
;;
(*  [a, b] * {(-inf, c] u [d, _inf)}  *)
(*  rozbija mnozenie nierozlacznego przedzialu przez rozlazny przedzial na dwa przypadki mnozenia dwoch nierozlaczych przedzialow i laczy wyniki  *)
let mult_single_double a b =
	let (b_left, b_right) = b
	in
		let res_left = mult_single_single a b_left
		and res_right = mult_single_single a b_right
		in
			if fst res_left = neg_infinity && fst res_right = neg_infinity then
				(neg_infinity, 		max (snd res_left) (snd res_right) neg_infinity neg_infinity), (nan, nan)
			else if fst res_left = neg_infinity && fst res_right <> neg_infinity then
				(res_left, res_right)
			else (res_right, res_left)
;;
(*  {x * y:  x nalezy do a  i  y nalezy do b}  *)
(*  odpowiednio rozbija mnozenie przedzialow na mniejsze przypadki i laczy wyniki  *)
let razy (a: wartosc) (b: wartosc) =
	let Range(a_left, a_right) = a
	and Range(b_left, b_right) = b
	in
		let (res_left, res_right) = match (is_nan_range a_right, is_nan_range b_right) with
			(*  [a, b]*[c, d]  *)
			(true, true)   -> (mult_single_single a_left b_left), (nan, nan) |

			(*  [a, b] * ((-inf, c] u [d, +inf)) = ([a, b] * (-inf, c]) u ([a, b] * [d, +inf))  *)
			(true, false)  -> 
				if a_left = (0., 0.) then (0., 0.), (nan, nan) else mult_single_double a_left (b_left, b_right) |

			(*  ((-inf, a] u [b, +inf)) * [c, d] = ((-inf, a] * [c, d]) u ([b, +inf) * [c, d])  *)
			(false, true)  ->
				if b_left = (0., 0.) then (0., 0.), (nan, nan) else mult_single_double b_left (a_left, a_right) |

			(*  ((-inf, a] u [b, +inf)) * ((-inf, c] u [d, +inf)) = {(-inf, a] * ((-inf, c] u [d, +inf))} u {[b, +inf) * ((-inf, c] u [d, +inf))}  *)
			(false, false) ->
				if snd a_left > 0. || fst a_right < 0. || snd b_left > 0. || fst b_right < 0. then
					(neg_infinity, infinity), (nan, nan)
				else
					let sub_res_left = mult_single_double a_left (b_left, b_right)
					and sub_res_right = mult_single_double a_right (b_left, b_right)
					in
						(neg_infinity,		max (snd (fst sub_res_left)) (snd (fst sub_res_right)) neg_infinity neg_infinity),
						(min (fst (snd sub_res_left)) (fst (snd sub_res_right)) infinity infinity,		infinity)
		(*  laczy (-inf, b] i [c, +inf), jesli c >= b  *)
		in
			if fst res_right <= snd res_left then Range((neg_infinity, infinity), (nan, nan)) else Range(res_left, res_right)
;;


(*  dla danych trzech nierozdzielnych przedzialow [Ai, Bi] sortuje je po ich najmniejszej wartosci (Ai), zakladamy, ze (nan, nan) jest wiekszy od kazdego przedzialu  *)
let sort_ranges a b c =
	if is_nan_range c then
		if is_nan_range b then (a, b, c) else
		if fst a <= fst b then(a, b, c) else (b, a, c)
	else
		if fst a <= fst b && fst a <= fst c then if fst b <= fst c then (a, b, c) else (a, c, b)
		else if fst b <= fst a && fst b <= fst c then if fst a <= fst c then (b, a, c) else (b, c, a)
		else if fst a <= fst b then (c, a, b) else (c, b, a)
;;
(*  laczy trzy nierozdzielne przedzialy  *)
let merge_ranges a b c = 
	let (res_left, res_right) = 
		if fst b <= snd a then (fst a, snd b), (nan, nan)
		else a, b
	in
		if is_nan_range c then (res_left, res_right)
		else 
			match is_nan_range res_right with
				false -> res_left, (fst res_right, max (snd res_right) (snd c) nan nan) |
				true  -> 
					if fst c <= snd res_left
						then (fst res_left, snd c), (nan, nan)
						else res_left, c
;;
(*  zwraca znak danej liczby  *)
let rec sign value = 
	if value > 0. then 1.
	else if value = 0. then sign (1. /. value)
	else (-1.)
;;
(*  zwraca wynik z dzielenia a/b lub (+-0 <=> a = 0 && b = +- infinity) lub (+-infinity <=> a = +-inf && b = +-inf)  *)
let safe_div a b = 
	if a = 0. then
		if abs b = infinity || b = 0.
		then (sign b) *. a
		else a /. b
	else if abs a = infinity && abs b = infinity then (sign a) *. (sign b) *. infinity
	else a /. b
;;
(*  [q, w] / [e, r], gdzie {e, r} sa tego samego znaku, wynik nigdy nie bedzie rozdzielny  *)
(*  zwraca wynik dzielenie wedlug tresci zadania dwoch nierozdzielnych przedzialow  *)
let div_single_single a b =
	if b = (0., 0.) then (nan, nan)
	else
		let (q, w) = a
		and (e, r) = b
		in
			(min (safe_div q e) (safe_div q r) (safe_div w e) (safe_div w r),
			max (safe_div q e) (safe_div q r) (safe_div w e) (safe_div w r))
;;
(*  a i b sa przedzialami rozlacznymi, jesli konce b maja inne znaki rozbija b w 0. na dwa przedzialy i zwraca polaczone wyniki dwoch wywolan dzielenia (a / podprzedzial b)  *)
let helper_single_single a b = 
	let range_one = if sign (fst b) *. sign (snd b) = -1. then div_single_single a (fst b, -0.) else div_single_single a b
	and range_two = if sign (fst b) *. sign (snd b) = -1. then div_single_single a (0., snd b) else (nan, nan)
	in
		let (one, two, _) = sort_ranges range_one range_two (nan, nan)
		in merge_ranges one two (nan, nan)
;;
(*  a - przedzial nierozlaczny, b - rozlaczny, rozbija przedzial b na trzy nierozlaczne podprzedzialy i zwraca polaczony wynik dzielenia a przez nie  *)
let div_single_double a b = 
	let (b_left, b_right) = b
	in
		let range_one = (neg_infinity, 	min (snd b_left) (-0.) nan nan)
		and range_two = (max (fst b_right) 0. nan nan, 	infinity)
		and range_three =
			if snd b_left > 0. then (0., snd b_left)
			else if fst b_right < 0. then (fst b_right, (-0.))
			else (0., 0.)
		in
			let res_one = div_single_single a range_one
			and res_two = div_single_single a range_two
			and res_three = div_single_single a range_three
			in
				let (one, two, three) = sort_ranges res_one res_two res_three
				in merge_ranges one two three
;;
(*  zapewnia, ze zera nierozdzielnego przedzialu maja dobre znaki  *)
let correct_zeros_single range = 
	if fst range < 0. && snd range = 0. then (fst range, (-0.))
	else if fst range = 0. && snd range > 0. then (0., snd range)
	else range
;;
(*  zapewnia, ze zera rozdzielnego przedzialu maja dobre znaki  *)
let correct_zeros_double range = 
	let (range_left, range_right) = range
	in
		if snd range_left = 0. then ((fst range_left, (-0.)), range_right)
		else if fst range_right = 0. then (range_left, (0., snd range_right))
		else range
;;
(*  {x / y:  x nalezy do x  i  y nalezy do b}  *)
(*  oblicza inv = ([1., 1.] / b) i zwraca (a * inv)  *)
let podzielic (a: wartosc) (b: wartosc) =
  let Range(b_left, b_right) = b;
	in
		match is_nan_range b_right with
			true  -> 
				let ((q, w), (e, r)) = helper_single_single (1., 1.) (correct_zeros_single b_left)
				in 
					razy a (nowy_przedzial q w e r) |
			false -> 
				let ((q, w), (e, r)) = div_single_double (1., 1.) (correct_zeros_double (b_left, b_right))
				in
					razy a (nowy_przedzial q w e r)
;;
