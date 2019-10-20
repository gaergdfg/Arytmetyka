open Util

type wartosc = {
	left: float;
	right: float
}


(*---------------------------Constructors---------------------------*)
let wartosc_dokladna a p = {
	left = a -. (abs a) *. p;
	right = a +. (abs a) *. p
}

let wartosc_od_do (a: float) (b: float) = {
	left = a;
	right = b
}

let wartosc_dokladna (a: float) = {
	left = a;
	right = a
}


(*---------------------------Queries---------------------------*)
let in_wartosc range x = 
	if x >= range.left && x <= range.right then true
	else false;;

let min_wartosc range = range.left;;

let max_wartosc range = range.right;;

let sr_wartosc range = 
	if abs range.left = infinity || abs range.right = infinity then nan
	else (range.left +. range.right) /. 2.;;


(*---------------------------Operations---------------------------*)
let plus a b = {
	left = a.left +. b.left;
	right = a.right +. b.right
}

let minus a b = {
	left = a.left -. b.right;
	right = a.right -. b.left
}

let razy a b = 
	let q = a.left
	and w = a.right
	and e = b.left
	and r = b.right
	in {
		left = min (q *. e) (q *. r) (w *. e) (w *. r);
		right = max (q *. e) (q *. r) (w *. e) (w *. r)
	}

(* let podzielic a b =
	??? *)
