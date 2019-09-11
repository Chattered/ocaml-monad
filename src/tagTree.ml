type ('tag,'a) node =
    Node of 'a * ('tag * ('tag,'a) node) LazyList.t

module type Tree =
sig
  type tag
  include Monad.BaseCollectionM
  include Applicative.Base with type 'a m := 'a m
  val count      : 'a m -> int
  val to_list    : 'a m -> 'a LazyList.t
  val collapse   : 'a m -> 'a m
  val with_tags  : 'a m -> (tag list * 'a) m
  val branch     : tag LazyList.t -> tag m
  val print      : ('a -> unit) -> 'a m -> unit
  val to_forced  : 'a m -> 'a m
end

module Make(Tag:sig
  type t
  val cmp   : t -> t -> bool
  val print : t -> unit
end) =
struct
  open LazyList
  open Printf

  type tag  = Tag.t
  type 'a m = (Tag.t,'a LazyList.t) node

  let return x = Node (singleton x, nil)

  let branch bs =
    if is_empty (drop 1 bs) then
      Node (bs, nil)
    else Node (nil, (map (fun b -> (b,return b)) bs))

  let zero () = Node (nil, nil)

  let rec null (Node (xs, cs)) =
    is_empty xs && all null (map snd cs)

  let rec count (Node (xs, cs)) =
    let f (_,t) = count t in
    length xs + fold_left (+) 0 (map f cs)

  (* Helper functions for join and plus. *)
  let map2 f xs = map (fun (x,y) -> (x, f y)) xs

  let rec map_tree f (Node (xs,cs)) =
    Node (f xs, map2 (map_tree f) cs)

  (* All elements that can be removed from xs together with the remaining list. *)
  let pinch xs =
    let rec loop preds xs =
      lazy
        (match next xs with
            Nil -> Nil
          | Cons (x',xs') ->
            Cons ((x', preds ^@^ xs'), loop (preds ^@^ singleton x') xs'))
    in loop nil xs

  let with_sibling_tags cs =
    map (fun (c, cs) -> c, map fst cs) (pinch cs)

  (* The right subtree T' is inserted at the leaf positions of the
     left tree T with the following simplifications:

     1) If T' has a subtree t' whose tag appears on a containing tree
     t'', then we replace t'' with f(t'') + t' where f(t'') is the
     result of deleting t' and its parent from t''.

     2) If T' has a subtree t' whose tag is an ancestor's sibling,
     then t' is deleted.
  *)
  let combine, simplify =
    let dest_tree tree =
      lazy (let Node (root, _) = Lazy.force tree in next root),
      lazy (let Node (_, cs)   = Lazy.force tree in next cs) in

    let rec simp_children ancestor_tags uncle_tags tagged_xs = lazy
      (let tagged_xs =
         filter (fun (tag, _) ->
           not (mem ~cmp:Tag.cmp tag uncle_tags))
           tagged_xs in
       let weaks =
         filter (fun (tag, _) -> mem ~cmp:Tag.cmp tag ancestor_tags)
           tagged_xs in
       if is_empty weaks then
         Node (nil, tagged_xs)
       else
         fold_right1 (comb (^@^) ancestor_tags uncle_tags)
           (map snd weaks))

    and simplify ancestor_tags uncle_tags (Node (x, tagged_xs)) =
      let x', tagged_xs' = dest_tree
        (simp_children ancestor_tags uncle_tags tagged_xs)
      in Node (x ^@^ x',
               map2 (simplify ancestor_tags uncle_tags)
                 tagged_xs')

    and comb : 'a 'c. ('a LazyList.t -> 'b LazyList.t -> 'c) ->
    Tag.t LazyList.t ->
    Tag.t LazyList.t ->
    (Tag.t, 'a LazyList.t) node ->
    (Tag.t, 'b LazyList.t) node Lazy.t -> (Tag.t, 'c) node
    = fun f ancestor_tags uncle_tags tree_x tree_y ->
      let Node (x,tagged_xs) = tree_x in
      let y = lazy (let Node (y, _) = Lazy.force tree_y in next y) in
      let tagged_ys =
        lazy (let Node (_,tagged_ys) = Lazy.force tree_y
              in next tagged_ys)
      in Node (f x y,
               lazy
                 (if is_empty tagged_xs then
                     let Node (_, tagged_ys') =
                       simplify ancestor_tags uncle_tags (Lazy.force tree_y)
                     in next (map2 (map_tree (f nil)) tagged_ys')
                  else
                     next
                       (map
                          (fun ((tag, c), sibling_tags) ->
                            (tag,
                             let ancestor_tags = tag ^:^ ancestor_tags in
                             let uncle_tags = sibling_tags ^@^ uncle_tags in
                             comb f ancestor_tags uncle_tags c
                               (simp_children
                                  ancestor_tags uncle_tags tagged_ys)))
                          (with_sibling_tags tagged_xs))))

    in (fun f tree_x tree_y -> comb f nil nil tree_x tree_y), simplify

  let compose f g x = f (g x)

  let combine_down f tf tx =
    let rec push_down (Node ((xs, ys, zs), cs)) =
      Node ((xs, ys, zs),
            map2
              (compose
                 (map_tree (fun (xs', ys', zs') ->
                   (xs', ys', zs' ^@^ f xs' ys ^@^ f xs ys')))
                 push_down) cs) in
    map_tree (fun (_,_,x) -> x)
      (push_down (combine (fun xs ys -> xs, ys, f xs ys) tf tx))

  let (<*>) ft xt = combine_down apl ft (lazy xt)

  let lplus t t' = combine (^@^) t t'

  let bind t f =
    let rec join (Node (t, cs)) =
      lplus t (lazy
                 (Node (nil, map (fun ((tag,c), rst) ->
                   tag, simplify (singleton tag) (map fst rst)
                     (join c))
                   (pinch cs))))
    in join (map_tree (fun xs -> fold_right lplus (zero ()) (map f xs)) t)

  (* let bind t f = *)
  (*   map_tree (concat_map root) (map_tree (map f) t) *)

  (* let bind t f = *)
  (*   let join t = *)
  (*     let only_child tag c = *)
  (*       Node (nil, (tag,c) ^:^ singleton (tag,c)) in *)
  (*     let rec unfolds (Node (t, cs)) = *)
  (*       lazy (Cons (t,concat_map *)
  (*                     (fun (tag, c) -> *)
  (*                        map (only_child tag) (unfolds c)) cs)) in *)
  (*       fold_right1 lplus (emptied ^:^ unfolds t) *)
  (*   in join (map_tree (fun xs -> fold_right lplus (zero ()) (map f xs)) t) *)

  (* Find difference between trees respecting assumption strength. The *)
  (* right tree is assumed to be larger and its topology is preserved. *)
  let difference p t1 t2 =
    let rec filter_down (Node ((xs, ys), cs)) =
      Node (LazyList.difference p xs ys,
            map2
              (fun c -> map_tree (fun xs' -> difference p xs' ys)
                (filter_down c)) cs) in
    filter_down (combine (fun xs ys -> xs, ys) t1 (lazy t2))

  let rec nub p (Node (xs, cs)) =
    let xs' = LazyList.nub p xs in
    Node (xs', map2 (compose
                       (map_tree (fun ys -> LazyList.difference p ys xs'))
                       (nub p)) cs)

  let rec maxima p (Node (xs, cs)) =
    let xs' = LazyList.maxima p xs in
    Node (xs', map2 (compose
                       (map_tree (fun ys -> LazyList.difference p ys xs'))
                       (maxima p)) cs)

  let rec unique ?(cmp = (=)) (Node (xs, cs)) =
    let xs' = LazyList.unique ~cmp xs in
    Node (xs', map2 (compose
                       (map_tree (fun ys -> LazyList.difference cmp ys xs'))
                       (unique ~cmp)) cs)

  let rec with_tags (Node (xs, cs)) =
    Node (map (fun x -> [], x) xs,
          map (fun (t,c) -> t,(map_tree (map (fun (ts,x) -> (t::ts), x))
                                 (with_tags c))) cs)

  let rec to_list (Node (xs, cs)) =
    map (fun x -> x) xs
    ^@^ concat_map (fun (_, c) -> to_list c) cs

  let collapse t = Node (to_list t, nil)

  let print p t =
    let rec print i (Node (xs, cs)) =
      let indent () =
        for _ = 0 to i do
          printf " "
        done
      in
      iter (fun x -> indent (); p x; printf " ") xs;
      printf "\n";
      iter
        (fun (t,c) ->
          for _ = 0 to i do
            printf " "
          done;
          indent (); Tag.print t; printf ": ";
          printf "\n";
          print (i + 2) c)
        cs;
    in print 0 t

  let rec to_forced (Node (x,cs)) =
    if is_forced cs then
      Node (x, map2 to_forced cs)
    else Node (x, nil)

end
