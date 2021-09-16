let rec factorielle n= match n with
|0->1
|_->factorielle(n-1)*n

let rec pascal n k = match k with
|a when a=n->1
|0->1
|_-> pascal (n-1) k + pascal (n-1) (k-1);;

let rec puissance a n= match n with
|0->1
|_-> (puissance a (n-1))*a;;

let rec puissance_2 a n= match n with
|0->1
|k when (k mod 2)=0 -> let x= puissance a (n/2) in x*x
|_-> let x= puissance a (n/2) in x*x*a

let rec puissance_3 a n = match n with
|k when k mod 2 =0-> let x= puissance a (n/2) in x*x
|k when k mod 3 =0-> let x= puissance a (n/3) in x*x*x
|k when k mod 5 =0-> let x= puissance a (n/5) in x*x*x*x
|k when k mod 7 =0-> let x= puissance a (n/7) in x*x*x*x*x
|_-> (puissance_3 a (n-1)) *a;;

let rec knuth n a b = match b with
|0->1
|_-> knuth (n-1) a (knuth n a (b-1));;