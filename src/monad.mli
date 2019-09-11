(** The monad library.

    {b Introduction}

    Monads in ocaml, as defined in the batteries library and lwt, are defined
    narrowly in terms of a type constructor, and two functions, [return] and
    [bind]. This misses the [i abstraction], which lies in the ability to write
    functions that apply generally to [i all] monads. This library defines modules
    for such functions.

    @author Phil Scott
 *)

(** {6 Base Modules}*)

module type Monoid =
  sig
    type t
    val zero : unit -> t
    val plus : t -> t -> t
  end

(** Monads with additional monoid structure. *)
module type BasePlus =
  sig
    include BatInterfaces.Monad
    val zero : unit -> 'a m
    val plus : 'a m -> 'a m -> 'a m

    (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. I have
      provided this so that streams can be implemented more efficiently. *)
    val null : 'a m -> bool
  end

(** LazyPlus is another base module useful when the monad is a lazy data
    structure. We then allow the plus operation to be non-strict in its second
    argument, which makes it possible to use functions such as
    {! Monad.lsum} lazily. This is what you want for lazy lists. *)
module type BaseLazyPlus =
  sig
    include BatInterfaces.Monad
    val zero  : unit -> 'a m
    val lplus : 'a m -> 'a m Lazy.t -> 'a m

    (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *)
    val null  : 'a m -> bool
  end

(** {6 Library Types } *)

(** Your basic library functions for monads. *)
module type Monad =
  sig
    include BatInterfaces.Monad
    include Applicative.Applicative with type 'a m := 'a m

    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
    val (>=>) : ('a -> 'b m) -> ('b -> 'c m) -> ('a -> 'c m)
    val (<=<) : ('b -> 'c m) -> ('a -> 'b m) -> ('a -> 'c m)
    val join  : 'a m m -> 'a m
    val filter_m : ('a -> bool m) -> 'a list -> 'a list m
    val onlyif : bool -> unit m -> unit m
    val unless : bool -> unit m -> unit m
    val ignore : 'a m -> unit m
  end

(** Library functions for monads with additional monoid structure. *)
module type MonadPlus =
  sig
    include BasePlus
    include Monad with type 'a m := 'a m

    val filter : ('a -> bool) -> 'a m -> 'a m
    val of_list : 'a list -> 'a m
    val sum  : 'a list m -> 'a m
    val msum : 'a m list -> 'a m
    val guard : bool -> unit m

    (** Generalises matrix transposition. This will loop infinitely if
  {! BasePlus.null} cannot answer [true] for [zero]es. *)
    val transpose : 'a list m -> 'a m list

  end

(** This is the counterpart for the lazy version of {! BasePlus}. *)
module type LazyPlus =
  sig
    include BaseLazyPlus
    include MonadPlus with type 'a m := 'a m

    val of_llist  : 'a LazyList.t -> 'a m
    val lsum      : 'a LazyList.t m -> 'a m
    val lmsum     : 'a m LazyList.t -> 'a m

    (** Generalises matrix transposition. You don't necessarily have to worry about
  correctly implementing {! BaseLazyPlus.null} for this function, since the return
  value can happily be infinite. *)
    val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
  end

(** {6 Library Creation} *)
module Make(M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m

module MakePlus (M : BasePlus) : MonadPlus with type 'a m = 'a M.m

module MakeLazyPlus (M : BaseLazyPlus) : LazyPlus with type 'a m = 'a M.m

(** {6 Specific monads} *)

(** The lazy monad. Automatically wraps calls lazily and forces as needed. *)
module LazyM : Monad with type 'a m = 'a Lazy.t

module List : MonadPlus with type 'a m = 'a list

module LazyListM : LazyPlus with type 'a m = 'a LazyList.t

module Option : MonadPlus with type 'a m = 'a option

module Retry(E : sig
                   type e
                   type arg
                   type tag
                   val defaultError : e
                 end) :
sig
  include MonadPlus
  type 'a err = Error  of (E.tag * (E.arg -> 'a m)) list * E.e
              | Result of 'a
  val throw     : E.e -> 'a m
  val catch     : 'a m -> (E.e -> 'a m) -> 'a m
  val add_retry : E.tag -> (E.arg -> 'a m) -> 'a m -> 'a m
  val run_retry : 'a m -> 'a err
end

(** For the incorruptible programmer:

    A continuation of type 'a has type [('a -> 'r) -> 'r]

    The first argument to this continuation is a final value of type 'r which
    depends on some intermediate value of type 'a. In other words, we can
    understand the continuation as a result of type 'r which depends on the {i
    future} of an intermediate value.

    The function [return] just {i throws} its return value to the future in
    order to produce the final result. The [bind] intercedes in the future,
    inserting a computation.

    Call-with-current-continuation allows one to effectively reflect into one's own
    future. That is, callCC is a computation which depends on another computation
    taking the future as a first-class value. One can store this future, and at any
    time, throw it a return value to reinstate it.

    If you are into the Curry-Howard Isomorphism,
    call-with-current-continuation has a type which corresponds to a law of
    classical logic (Pierce's Law). Writing your code in the continuation
    monad corresponds to embedding classical logic intuitionistically. Allowing
    callCC corresponds to assuming a classical hypothesis.
 *)
module Continuation(T : sig type r end) :
sig
  include Monad with type 'a m = ('a -> T.r) -> T.r

  val callCC : (('a -> 'r m) -> 'a m) -> 'a m
end

module Reader(T : sig type t end) :
sig
  include Monad

  val read : T.t m
  val run  : T.t -> 'a m -> 'a
end

module Writer(M : Monoid) :
sig
  include Monad

  val listen : 'a m -> (M.t * 'a) m
  val run   : 'a m -> M.t * 'a
  val write : M.t -> unit m
end

module State(T : sig type s end) :
sig
  include Monad

  val read   : T.s m
  val write  : T.s -> unit m
  val modify : (T.s -> T.s) -> unit m
  val run    : 'a m -> T.s -> (T.s * 'a)
  val eval    : 'a m -> T.s -> 'a
end

module Error(E : sig type e val defaultError : e end) :
sig
  type 'a err = Error  of E.e
              | Result of 'a
  include MonadPlus

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val run_error : 'a m -> 'a err
end

(** Monads for collections. Stream is the current use-case for this, since it
    is parameterised on a collection monad (like [list]). *)
module type BaseCollectionM =
  sig
    include BaseLazyPlus

    (** [difference p xs ys] removes all elements from [xs] which are less than or
    equal to some element in [ys], according to the partial order [p]. *)
    val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m

    (** [unique eq xs] removes duplicates according to the function [cmp]. *)
    val unique : ?cmp:('a -> 'a -> bool) -> 'a m -> 'a m

    (** [maxima p xs] leaves only the maximal elements according to the
  partial order [p]. *)
    val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m

    (** [nub p xs ys] is the same as maxima, but some values might be treated as
  greater than others depending on their position in the collection.

  For instance, with lists, we treat elements that occur earlier in the list
  as always greater than elements that occur later, otherwise we use [p]. This
  is really just an optimisation: in order to retrieve any value from [maxima p
  xs], the [maxima] function will have had to seen every value. Not so with
  [nub].*)
    val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
  end

(** Streams supporting fair and unbounded search.

    Spivey has a nice and authoratitive paper on this, understanding streams as
    a breadth-first search. If that description appeals to you, I recommend his
    paper. Personally, I found it helpful to understand streams a bit
    differently, as concurrent processes that go off and discover data.

    First, we have {i generations}, which are collections of zero or more
    data. The data in each generation is discovered simultaneously, and
    therefore, it doesn't make much sense to have any ordering imposed on the
    generations. In fact, the lack of an ordering is essential for getting the
    associativity laws of the monad, and is why Spivey's paper works with bags.

    Now a stream is just a lazy list of generations. We want to think of the
    indices into these streams as temporal indices. So the first generation in
    the lazy list was discovered at time 0. The second was discovered at time
    1. The third was discovered at time 2. And so on.

    Thinking this way helped me figure out [return] and [join]. Here,
    [return x] gives you the process which immediately discovers [x] and then
    terminates.

    With [join xss], we should interpret the [xss] as a process which discovers
    {i other processes}. When we join them, we ask the outer process to fork
    each inner process as soon as it is discovered, and then merge in all the
    values found by the forked processes. For instance, if [xs] discovers a
    process [p] at time 5, and in turn, [p] discovers the string "hello world!"
    at time 13, then [join xs] discovers "hello world!" at time 5 + 13 = 18.

    Now we can understand [bind], realising that it is just the result of doing
    a [map] and then a [join]. The expression [bind xs f] forks processes [f]
    which depend on a discovered value [x], and then merges back all their values.

    Technically, all our streams should be infinite, and the processes should
    run forever, but this gave me sad performance in my theorem proving
    code. So for efficiency, our streams can be truncated, which represents a
    process that terminates.

    Note that it is impossible to define a general {! BaseLazyPlus.null} predicate
    for streams, because we would have to be able to decide whether an infinite
    stream of values consists entirely of the empty generation. Turing says that's
    impossible, and I believe him. As a crude approximation, then, we have it that {!
    BaseLazyPlus.null} returns [true] just for the special case that its input is an
    empty lazy list. *)
module type Stream =
  sig
    type 'a t
    include LazyPlus with type 'a m = 'a t Lazy.t

    (** The sum of the stream [\[f x, f (f x), f (f (f x)),...\]] *)
    val iterate : ('a m -> 'a m) -> 'a m -> 'a m

    (** Delay a stream by one time step. This is needed when you write recursive
  streams and you have to avoid deadlock. The nice thing about Ocaml here is that
  it will generally detect deadlock for you, announcing to you that you're
  writing viciously circular lists! *)
    val delay : 'a m -> 'a m

    (** Terminate discovery at some depth. *)
    val to_depth : int -> 'a m -> 'a m

  end

(** The union of streams and collections. *)
module type StreamC =
  sig
    include Stream
    include BaseCollectionM with type 'a m := 'a m
  end

(** {6 Transformers} *)

module LazyT(M : BatInterfaces.Monad) :
sig
  include Monad with type 'a m = 'a Lazy.t M.m
  val lift   : 'a M.m -> 'a m
end

(** The list monad transformer will add non-determinism to computations. I
    have not provided a transformer for lazy lists, since I'm not yet sure how
    to implement it. It would probably need a lazy version of map_m, but it's
    not clear to me how to write this, since whether the computations are
    strict will determine whether the argument has to be completely forced. *)
module ListT(M : BatInterfaces.Monad) :
sig
  include Monad with type 'a m = 'a list M.m
  val lift   : 'a M.m -> 'a m
end

module OptionT(M : BatInterfaces.Monad) :
sig
  include Monad with type 'a m = 'a option M.m
  val lift   : 'a M.m -> 'a m
end

module ErrorT(E : sig type e val defaultError : e end)(M : BatInterfaces.Monad) :
sig
  type 'a err = Error  of E.e
              | Result of 'a
  include Monad

  val throw     : E.e -> 'a m
  val catch     : 'a m -> (E.e -> 'a m) -> 'a m
  val lift      : 'a M.m -> 'a m
  val run_error : 'a m -> 'a err M.m
end

module StateT(T : sig type s end)(M : BatInterfaces.Monad) :
sig
  include Monad

  val read   : T.s m
  val write  : T.s -> unit m
  val modify : (T.s -> T.s) -> unit m
  val run    : 'a m -> T.s -> (T.s * 'a) M.m
  val eval   : 'a m -> T.s -> 'a M.m
  val lift   : 'a M.m -> 'a m
end

module WriterT(Mon : Monoid)(M : BatInterfaces.Monad) :
sig
  include Monad
  val listen : 'a m -> (Mon.t * 'a) m
  val write  : Mon.t -> unit m
  val run    : 'a m -> (Mon.t * 'a) M.m
  val lift   : 'a M.m -> 'a m
end

(** {6 Transformers on Collections } *)
(** Sometimes, you might want to transform a collection monad, in such a way
    that functions such as {! BaseCollectionM.unique} behave in a sensible
    way by regarding None as smaller than any Some. Each transformer provides
    a function [cmp_on] with which the collection functions such as
    [BaseCollectionM.difference] are implemented. We also provide the [list]
    function for transformers. *)

module CollectionOpt(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = 'a option C.m
  include Monad with type 'a m := 'a m

  (** [cmp_on p] is a partial order satisfying:
         [None   = None]
         [None   < Some _]
         [Some x < Some y <=> x < y]
   *)
  val cmp_on : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

  val lift   : 'a C.m -> 'a m
end

module CollectionWriter(Mon : sig
                                include Monoid
                                val cmp : t -> t -> bool
                              end)(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = (Mon.t * 'a) C.m
  include Monad with type 'a m := 'a m
  val write : Mon.t -> unit m

  val run   : 'a m -> (Mon.t * 'a) C.m

  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [W.t]. *)
  val cmp_on : ('a -> 'a -> bool) -> (Mon.t * 'a) -> (Mon.t * 'a) -> bool

  val lift   : 'a C.m -> 'a m
end

module CollectionState(T :
                         sig
                           type s
                           val cmp : s -> s -> bool
                         end)(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = T.s -> (T.s * 'a) C.m
  include Monad with type 'a m := 'a m
  val read   : T.s m
  val write  : T.s -> unit m

  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [T.s]. *)
  val cmp_on : ('a -> 'a -> bool) -> (T.s * 'a) -> (T.s * 'a) -> bool

  val lift   : 'a C.m -> 'a m
end

(** Create a stream monad from an arbitrary inner monad, which computes the
    values discovered in each generation. This is pretty abstract, since we're
    not requiring that the inner monad is a {i collection}. There is a
    constraint here, since we're strictly supposed to disallow a monad
    collection where order of elements matters. I think we can characterise
    this abstractly by saying that the plus operation on the inner monad must
    be commutative, but don't take my word for it!
 *)
module MakeStream(M : sig
                        include BaseLazyPlus
                        include Applicative.Base with type 'a m := 'a m
                      end) :
sig
  include Stream with type 'a t = 'a M.m LazyList.node_t
                  and type 'a m = 'a M.m LazyList.t
  include Monad with type 'a m := 'a m
end

(** Here, we create a stream monad from a definite collection monad
    {! Monad.BaseCollectionM}. The inner monad will be used to represent the
    generations in the stream. The order of elements in each generation should
    not matter, so you might want to use a set or a bag. If you want to live
    life on the edge, just remember that your code should not depend on the
    order of elements within generations (you can, of course, depend on the
    order that generations appear in the stream). You can enforce this
    constraint by performing, say, a sort on each generation.  *)
module MakeStreamC(M : sig
                         include BaseCollectionM
                         include Applicative.Base with type 'a m := 'a m
                       end) :
sig
  include StreamC with type 'a t = 'a M.m LazyList.node_t
                   and type 'a m = 'a M.m LazyList.t
end
