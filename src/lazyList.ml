(* Lazy lists do not have the same support in Ocaml as in Haskell. The folds
   are eager, as are functions implemented in terms of folds. The concat
   function is documented as lazy but the outer list is eagerly evaluated,
   meaning that
      concat (from (fun () -> singleton 1))
   will not terminate, and indeed
      concat (from () -> nil))
   which in Haskell is undefined but constant space when evaluated, will leak
   in Ocaml.
   Enumerations, though destructive and not thread-safe, have better
   properties:
       get (concat () (repeat (empty ())))
   is non-terminating but constant in space. *)

include BatLazyList
module Bl = BatList

let of_delay xs = Lazy.from_fun (fun () -> next (xs ()))

let next l = Lazy.force l

let singleton x = x ^:^ nil

let repeat x = from (fun () -> x)

let is_non_empty xs =
  match next xs with
      Nil -> false
    | _ -> true

let rec take n xs =
  lazy
    (if n <= 0 then Nil
     else match next xs with
         Nil -> Nil
       | Cons (y, ys) -> Cons (y, take (n-1) ys))

let drop n xs =
  let rec loop n xs =
    if n <= 0 then next xs
    else match next xs with
        Nil -> Nil
      | Cons (_, ys) -> loop (n-1) ys
 in lazy (loop n xs)

let rec take_while p xs =
  lazy (match next xs with
            Nil -> Nil
          | Cons (y, ys) when p y -> Cons (y, take_while p ys)
          | Cons (_, _) -> Nil)

let rec zip_with f xs ys =
  lazy
    (match (next xs, next ys) with
         Nil, _   -> Nil
       | _, Nil -> Nil
       | Cons (x, xss),Cons (y, yss) ->
           Cons (f x y, zip_with f xss yss))

let rec combine_with f xs ys =
  lazy
    (match (next xs, next ys) with
         Nil, Nil   -> Nil
       | _, Nil -> raise (Different_list_size "combine")
       | Nil, _ -> raise (Different_list_size "combine")
       | Cons (x, xss),Cons (y, yss) ->
           Cons (f x y, combine_with f xss yss))

let zip xs ys = zip_with (fun x y -> (x,y)) xs ys

let fold_right f b xs =
  let rec loop xs = match next xs with
      Nil -> b
    | Cons (x, xs) -> f x (lazy (loop xs)) in
    loop xs

let fold_right1 f xs =
  let rec loop xs = match next xs with
      Nil -> raise Empty_list
    | Cons (x, xs) -> match next xs with
          Nil -> x
        | _   -> f x (lazy (loop xs))
  in loop xs

let fold_left1 f xs =
  match next xs with
      Nil -> raise Empty_list
    | Cons (x, xs') -> fold_left f x xs'

let rec map_accum_l f acc xs =
  lazy (match next xs with
            Nil -> Nil
          | Cons (x, xs) ->
              let (acc', y) = f acc x in
                Cons (y, map_accum_l f acc' xs))

let rec scan f b xs =
  lazy (Cons (b, (lazy
                    (match next xs with
                         Nil -> Nil
                       | Cons (x,xs) -> next (scan f (f b x) xs)))))

let scan1 f xs =
  lazy (match next xs with
            Nil -> Nil
          | Cons (x, xs) -> next (scan f x xs))

let map_accum_l2 f acc xs =
  let g a (_, x) =
    let (c, y) = f a x in (next c, (c, y))
  in let mapAcc = map_accum_l g acc (map (fun x -> (lazy acc, x)) xs) in
    (lazy (next (last (lazy acc ^:^ map fst mapAcc))), map snd mapAcc)

let flatten xss =
  List.fold_left (^@^) nil xss

let concat xss =
  lazy (fold_right (fun xs ys -> next (xs ^@^ ys)) Nil xss)

let concat_map f xs = concat (map f xs)

let inits xs =
  let rec loop xs ys = match next xs with
      Nil -> Nil
    | Cons (x, xs') ->
        let zs = ys ^@^ (singleton x)
        in Cons (zs, lazy (loop xs' zs))
  in
    nil ^:^ lazy (loop xs nil)

let rec tails xs =
  lazy (match next xs with
            Nil -> Nil
          | Cons (_,ys) -> Cons (xs, tails ys))

let any p xs =
  try
    ignore (find p xs);
    true
  with Not_found -> false

let mem ?(cmp = (=)) x xs =
  let rec loop xs =
    match next xs with
        Nil                      -> false
      | Cons (y,_) when cmp x y -> true
      | Cons (_,ys)              -> loop ys
  in loop xs

let difference p xs ys =
  filter (fun x -> not (any (p x) ys)) xs

let union ?(cmp = (=)) xs ys =
  xs ^@^ (difference cmp ys xs)

let intersect ?(cmp = (=)) xs ys =
  filter (fun x -> any (cmp x) ys) xs

let intersect_class ?(cmp = (=)) xs ys =
  concat_map (fun x ->
                let ys' = filter (cmp x) ys in
                  if is_empty ys' then nil
                  else x ^:^ ys') xs

let rec interleave xs ys =
  lazy
    (match next xs with
         Nil -> next ys
       | Cons (x, xs) -> Cons(x, interleave ys xs))

let any p xs = is_non_empty (filter p xs)
let all p xs = is_empty (filter (fun x -> not (p x)) xs)

let subset ?(cmp = (=)) xs ys =
  all (fun x -> any (cmp x) ys) xs

let span p xs =
  let rec id_tails xs = match next xs with
      Nil -> Nil
    | Cons (x', xs') -> Cons ((x', xs), lazy (id_tails xs'))
  in let its = lazy (id_tails xs) in
    (take_while p (map fst its),
     concat (map snd (take 1 (drop_while (fun (x,_) -> p x) its))))

let partition p xs =
  let rec loop xs =
    match next xs with
        Nil -> Nil
      | Cons (x', xs') when p x'
          -> Cons ((singleton x', nil), lazy (loop xs'))
      | Cons (x', xs')
        -> Cons ((nil, singleton x'), lazy (loop xs'))
  in concat_map fst (lazy (loop xs)), concat_map snd (lazy (loop xs))

let rec range m n =
  lazy (if m > n then Nil else Cons (m, range (m + 1) n))

let apl fs xs =
  concat_map (fun f -> map f xs) fs

let unique ?(cmp = (=)) xs =
  let rec unique xs =
    lazy (match next xs with
              Nil -> Nil
            | Cons (x,xs) -> Cons (x, filter (fun y -> not (cmp x y)) (unique xs)))
  in unique xs

let rec maxima p xs =
  lazy (match next xs with
            Nil -> Nil
          | Cons (x, xs) when any (p x) xs -> next (maxima p xs)
          | Cons (x, xs) -> Cons (x, maxima p (filter (fun y -> not (p y x)) xs)))

let list_any p xs =
  let rec loop =
    function [] -> false
      | x :: xs -> p x || loop xs
  in loop xs

let nub p xs =
  let rec loop acc xs =
    lazy (match next xs with
        Nil -> Nil
      | Cons (x,xs) when list_any (p x) acc -> next (loop acc xs)
      | Cons (x,xs) ->
        let acc' = x :: BatList.filter (fun y -> not (p y x)) acc in
        Cons (x, loop acc' xs))
  in loop [] xs

let rec unzip xys =
  ((lazy (match next xys with
              Nil -> Nil
            | Cons ((x,_), xys') ->
                let (xs, _) = unzip xys' in
                  Cons (x, xs))),
   (lazy (match next xys with
              Nil -> Nil
            | Cons ((_,y), xys') ->
                let (_, ys) = unzip xys' in
                  Cons (y, ys))))

let cat_option xs = filter_map (fun x -> x) xs

let rec is_forced xs =
  if Lazy.is_val xs then
    match next xs with
        Cons (_,xs) -> is_forced xs
      | Nil         -> true
  else false

let rec to_forced xs =
  if Lazy.is_val xs then
    match next xs with
        Cons (x,xs) -> lazy (Cons (x,to_forced xs))
      | Nil -> nil
  else nil

let find_all eq set xs =
  let rec f set xs =
    if Bl.is_empty set then [] else
      match next xs with
          Nil -> []
        | Cons (y,ys) ->
          let matches,rst =
            Bl.partition (fun x -> eq x y) set in
          if Bl.is_empty matches
          then f set ys
          else y :: f rst ys
  in f set xs
