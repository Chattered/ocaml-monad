(** An alternative to the batteries lazy list library, with generally improved
    laziness.
    @author Phil Scott *)

(** {6 Types} *)

type 'a t     = ('a node_t) Lazy.t
and 'a node_t = Nil | Cons of 'a * 'a t

(** {6 Creation} *)

val nil : 'a t
val singleton : 'a -> 'a t

(** A constant stream of the input. *)
val repeat : 'a -> 'a t

(** A list whose elements come from unit-delayed expressions.*)
val from : (unit -> 'a) -> 'a t

(** Convert an eager list to a lazy one. *)
val of_list : 'a list -> 'a t

(** The lazy list of values [0..n] *)
val range : int -> int -> int t

(** Create a list using a function representing the body of a loop. The loop
    terminates when the body returns [None]. *)
val unfold : 'a -> ('a -> ('b * 'a) option) -> 'b t

(** Binary cons operator. *)
val ( ^:^ ) : 'a -> 'a t -> 'a t

(** {6 Access} *)

val hd : 'a t -> 'a

(** {6 Predicates} *)

val is_empty : 'a t -> bool
val is_non_empty : 'a t -> bool

(** Tests whether any element of the list is satisfied by a predicate. *)
val any : ('a -> bool) -> 'a t -> bool

(** Tests whether every element of the list is satisfied by a predicate. *)
val all : ('a -> bool) -> 'a t -> bool

(** {6 Transformations} *)

val cons : 'a -> 'a t -> 'a t

(** Append *)
val ( ^@^ ) : 'a t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

(** Maps a partial function. *)
val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t

(** Take elements to the first element for which the predicate fails. *)
val take_while : ('a -> bool) -> 'a t -> 'a t

(** Drop elements until the first element for which the predicate fails. *)
val drop_while : ('a -> bool) -> 'a t -> 'a t

(** [span p xs] is equivalent to [(take_while p xs, drop_while p xs)]. *)
val span : ('a -> bool) -> 'a t -> 'a t * 'a t

(** Splits a list into those elements which satisfy a predicate and those which
    don't. *)
val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)

(** Maps a binary function across corresponding elements of two lists. *)
val combine_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** Maps a binary function across corresponding elements of two lists,
    up to the length of the shortest list. *)
val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** Zip with pairing. *)
val zip : 'a t -> 'b t -> ('a * 'b) t

val unzip : ('a * 'b) t -> 'a t * 'b t

val fold_right : ('a -> 'b Lazy.t -> 'b) -> 'b -> 'a t -> 'b
val fold_right1 : ('a -> 'a Lazy.t -> 'a) -> 'a t -> 'a

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left1 : ('a -> 'a -> 'a) -> 'a t -> 'a

(** Folds but keeps successive elements. *)
val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
val scan1 : ('a -> 'a -> 'a) -> 'a t -> 'a t

(** Maps a function across a list whilst accumulating a state value. *)
val map_accum_l : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'b t

(** As map_accum_l, but also returns the final accumulated value. The input
    list will be forced only once, even if both components of the pair are
    forced. However, two passes of the list are still required. *)
val map_accum_l2 : ('acc -> 'a -> 'acc Lazy.t * 'b)
                          -> 'acc -> 'a t -> 'acc Lazy.t * 'b t

val concat : 'a t t -> 'a t

(** Concatenation eager in the outer list. *)
val flatten : 'a t list -> 'a t

(** As [fun f xss -> concat (map f xs)] *)
val concat_map : ('a -> 'b t) -> 'a t -> 'b t

(** The prefixes of a list starting from [nil]. *)
val inits : 'a t -> 'a t t

(** The suffixes of a list starting from the list itself. *)
val tails : 'a t -> 'a t t

val interleave : 'a t -> 'a t -> 'a t

(** Non-deterministic application in lists. *)
val apl : ('a -> 'b) t -> 'a t -> 'b t

(** {6 Set operations} *)

(** Tests whether every element in the first list is equivalent to the elements
    in the second list. *)
val subset : ?cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Remove duplicates from a list. *)
val unique : ?cmp:('a -> 'a -> bool) -> 'a t -> 'a t

(** Tests whether an element belongs to the list.*)
val mem : ?cmp:('a -> 'a -> bool) -> 'a -> 'a t -> bool

(** Based on a partial order. Any element in the first argument which is
    smaller than an element in the second is removed. *)
val difference : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t

val union : ?cmp:('a ->' a -> bool) -> 'a t -> 'a t -> 'a t

(** Retains elements in the first argument which are equivalent to some element
    in the second. *)
val intersect : ?cmp:('a ->' a -> bool) -> 'a t -> 'a t -> 'a t

(** Retains elements in the first argument which are equivalent to some element
    in the second, and vice versa. *)
val intersect_class : ?cmp:('a ->' a -> bool) -> 'a t -> 'a t -> 'a t

(** Filters out all [None]s and strips [Some]s. *)
val cat_option : 'a option t -> 'a t

(** Reduce a list to its maxima according to the partial-order. *)
val maxima : ('a -> 'a -> bool) -> 'a t -> 'a t

(** Haskell style nub. *)
val nub : ('a -> 'a -> bool) -> 'a t -> 'a t

(** {6 Miscellaneous} *)

val tl : 'a t -> 'a t
val length : 'a t -> int

(** Force the first node in a list.*)
val next : 'a t -> 'a node_t

(** The following function is sometimes useful for defining recursive lists. *)
val of_delay : (unit -> 'a t) -> 'a t

(** Loops a side-effecting procedure over the elements of a list. *)
val iter : ('a -> unit) -> 'a t -> unit

val to_list : 'a t -> 'a list
val enum : 'a t -> 'a BatEnum.t

(** True if the whole list has been forced. *)
val is_forced : 'a t -> bool

(** Retrieve as much of a lazy list as has been forced to far. *)
val to_forced : 'a t -> 'a t

(** Find all elements of a set. *)
val find_all : ('a -> 'b -> bool) -> 'a list -> 'b t -> 'b list
