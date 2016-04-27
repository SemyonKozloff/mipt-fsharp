module prac


type 'T list = Nil | Cons of 'T list 

let list = [ 1; 2; 3; 4; 5 ]

let rec length = function
    | [] -> 0
    | _ :: tail -> 1 + length tail



let rec foldr f init = function
    | [] -> init
    | head :: tail -> f head (foldr f init tail)
    
let sum = foldr (+) 0 
let prod = foldr (*) 1

let rec reduce f = function 
    | [] -> failwith "Cannot reduce"
    | [x] -> x
    | head :: tail -> f head (reduce f tail)



let rec map f = function 
    | [] -> []
    | head :: tail -> (f head) :: (map f tail)

//let map f = foldr (fun head tail -> (f head) :: tail) []

let rec filter p = function 
    | [] -> []
    | head :: tail when p head -> head :: (filter p tail)
    | head :: tail -> filter p tail

//let filter p = foldr (fun head tail -> if p head then head :: tail else tail) []

let rec primes = function 
    | [] -> []
    | head :: tail -> head :: primes(List.filter (fun x -> x % head <> 0) tail)

primes [2..100]



let rec foldl f init list = 
    let rec fold acc = function
        | [] -> acc
        | head :: tail -> fold (f head acc) tail
    fold init list 

foldr (fun h t -> h :: t) [] [1..10]
foldl (fun h t -> h :: t) [] [1..10]



type 'T tree = Leaf of 'T | Node of 'T * ('T tree list)

//let rec map f = function 
//   | Leaf (x) -> Leaf (f x)
//   | Node (x, l) -> Node (f x, List.map (map f) l) 



type 'T btree = Nil | Node of  'T * 'T btree * 'T btree

let rec treefoldr f i = function 
    | Nil -> i
    | Node (x, l, r) -> 
        let acc = treefoldr f i l
        treefoldr f (f acc x) r

let treefoldl f =
    let rec fold' acc = function 
        | Nil -> acc
        | Node (x, l, r) -> 
            fold' (f (fold' acc l) x) r
    fold'