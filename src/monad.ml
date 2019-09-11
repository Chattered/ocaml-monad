module Ll = LazyList
let (^:^) = Ll.(^:^)
let (^@^) = Ll.(^@^)

module type BasePlus =
sig
  include BatInterfaces.Monad
  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type BaseLazyPlus =
sig
  include BatInterfaces.Monad
  val zero  : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m
  val null  : 'a m -> bool
end

module type Monad =
sig
  include BatInterfaces.Monad
  include Applicative.Applicative with type 'a m := 'a m

  val (>>=)    : 'a m -> ('a -> 'b m) -> 'b m
  val (>=>) : ('a -> 'b m) -> ('b -> 'c m) -> ('a -> 'c m)
  val (<=<) : ('b -> 'c m) -> ('a -> 'b m) -> ('a -> 'c m)
  val join     : 'a m m -> 'a m
  val filter_m : ('a -> bool m) -> 'a list -> 'a list m
  val onlyif : bool -> unit m -> unit m
  val unless : bool -> unit m -> unit m
  val ignore : 'a m -> unit m
end

module type MonadPlus =
sig
  include BasePlus
  include Monad with type 'a m := 'a m
  val filter  : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum       : 'a list m -> 'a m
  val msum      : 'a m list -> 'a m
  val guard     : bool -> unit m
  val transpose : 'a list m -> 'a m list
end

module type LazyPlus =
sig
  include BaseLazyPlus
  include MonadPlus with type 'a m := 'a m
  val of_llist   : 'a LazyList.t -> 'a m
  val lsum       : 'a LazyList.t m -> 'a m
  val lmsum      : 'a m LazyList.t -> 'a m
  val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
end

module Make(M : BatInterfaces.Monad) =
struct
  include M

  let (>>=)       = bind
  let (>=>) g f x = g x >>= f
  let (<=<) f g x = g x >>= f

  let lift1 f x     = x >>= fun x -> return (f x)
  let lift2 f x y   = x >>= fun x -> lift1 (f x) y

  module Ap =
    Applicative.Make(struct
      include M
      let (<*>) f x  = lift2 (fun f x -> f x) f x
    end)

  include (Ap : Applicative.Applicative with type 'a m := 'a m)

  let join     m = m >>= (fun x -> x)

  let filter_m p =
    let rec loop m = function
      | []      -> lift1 List.rev m
      | (x::xs) -> loop (p x >>=
                           fun b -> m >>=
                               fun ys -> return (if b then (x::ys) else ys)) xs in
    loop (return [])

  let ignore m   = lift1 (fun _ -> ()) m
  let onlyif b m = if b then m else return ()
  let unless b m = if b then return () else m
end

module MakePlus(M : BasePlus) =
struct
  include Make(M)
  let zero ()  = M.zero ()
  let plus     = M.plus
  let null     = M.null
  let filter p xs =
    xs >>= fun x -> if p x then return x else zero ()
  let of_list xs = List.fold_left
    (fun x y -> plus x (return y))
    (zero  ()) xs
  let sum xs =
    xs >>=
      (fun xs -> List.fold_right plus (List.map return xs) (zero ()))
  let msum xs = List.fold_left plus (zero ()) xs

  let guard b = if b then return () else zero ()

  let rec transpose xs =
    let hds = sum (lift1 (BatList.take 1) xs) in
    if null hds then [] else
      hds :: transpose (lift1 (BatList.drop 1) xs)
end

module MakeLazyPlus(M : BaseLazyPlus) =
struct
  include MakePlus(struct
                     include M
                     let plus x y = M.lplus x (lazy y)
                   end)
  let lplus = M.lplus
  let lsum xs =
    xs >>=
      (fun xs -> Ll.fold_right lplus (zero ())
        (Ll.map return xs))

  let of_llist xs =
    Ll.fold_right lplus (zero ())
      (Ll.map return xs)

  let lmsum xs = Ll.fold_right lplus (zero ()) xs

  let filter p xs =
    xs >>= fun x -> if p x then return x else zero ()

  let rec ltranspose xs =
    lazy (let hds = lsum (lift1 (Ll.take 1) xs) in
            if null hds then Ll.Nil else
              Ll.Cons(hds, ltranspose (lift1 (Ll.drop 1) xs)))
end

module MakeStream(M : sig
    include BaseLazyPlus
    include Applicative.Base with type 'a m := 'a m
end) =
struct
  module ML = MakeLazyPlus(M)

  type 'a t = 'a ML.m LazyList.node_t

  let delay xs = M.zero () ^:^ xs

  module Base = struct
    type 'a m = 'a t Lazy.t

    let zero () = Ll.nil

    let rec lplus xss yss =
      let hd_yss = lazy (match Ll.next (Lazy.force yss) with
                           Ll.Nil -> ML.zero ()
                         | Ll.Cons (ys,_) -> ys)
      and tl_yss = lazy (match Ll.next (Lazy.force yss) with
                           Ll.Nil -> Ll.nil
                         | Ll.Cons (_,yss) -> yss)
      in lazy (match Ll.next xss with
                 Ll.Nil -> Ll.next (Lazy.force yss)
               | Ll.Cons (xs,xss) ->
                  Ll.Cons (ML.lplus xs hd_yss, lplus xss tl_yss))

    let plus xss yss = lplus xss (lazy yss)

    let null = Ll.is_empty

    let return x  = Ll.singleton (ML.return x)

    let bind xs f =
      let join xs =
        let rec shift xs =
          lazy (match Ll.next xs with
                  Ll.Nil -> Ll.Nil
                | Ll.Cons (x', xs') ->
                   Ll.next (plus
                              (Ll.map ML.join (ML.ltranspose x'))
                              (delay (shift xs')))) in
        shift xs
      in join (Ll.map (ML.lift1 f) xs)

    let (<*>) fs xs =
      let rec shift fs =
        lazy (match Ll.next fs with
                Ll.Nil -> Ll.Nil
              | Ll.Cons (f', fs') ->
                 Ll.next
                   (* Without the null check, we'll have a space leak as
               the empty generation is uselessly applied across xs. *)
                   (if ML.null f' then delay (shift fs')
                    else plus
                           (Ll.map (M.(<*>) f') xs)
                           (delay (shift fs')))) in
      shift fs
  end

  include MakeLazyPlus(Base)
  include (Applicative.Make(Base) : Applicative.Applicative with type 'a m := 'a m)

  let to_depth n = LazyList.take n

  let rec iterate f xs =
    plus xs (delay (lazy (Ll.next (iterate f (f xs)))))

end

module type BaseCollectionM =
sig
  include BaseLazyPlus
  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m
  val unique : ?cmp:('a -> 'a -> bool) -> 'a m -> 'a m
  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m
  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
end

module type Stream =
sig
  type 'a t
  include LazyPlus with type 'a m = 'a t Lazy.t
  val iterate : ('a m -> 'a m) -> 'a m -> 'a m
  val delay : 'a m -> 'a m
  val to_depth : int -> 'a m -> 'a m
end

module type StreamC =
sig
  include Stream
  include BaseCollectionM with type 'a m := 'a m
end

module MakeStreamC(M : sig
    include BaseCollectionM
    include Applicative.Base with type 'a m := 'a m
end) =
struct
  include MakeStream(M)
  module Mm = MakeLazyPlus(M)

  let nub p xs =
    let nub maxs x =
      let x' = M.nub p (M.difference p x maxs) in
        M.lplus (M.difference p maxs x') (lazy x'), x'
    in Ll.map_accum_l nub (M.zero ()) xs

  let maxima p xs =
    let maxima maxs x =
      let x' = M.maxima p (M.difference p x maxs) in
        M.lplus (M.difference p maxs x') (lazy x'), x'
    in Ll.map_accum_l maxima (M.zero ()) xs

  let unique ?(cmp = (=)) xs =
    let unique uniques x =
      let x' = M.difference cmp (M.unique ~cmp x) uniques
      in M.lplus (M.difference cmp uniques x') (lazy x'), x'
    in Ll.map_accum_l unique (M.zero ()) xs

  let difference p xss (yss : 'a m) =
    Ll.zip_with (M.difference p)
      xss
      (Ll.drop 1 (Ll.scan Mm.plus (M.zero ()) (yss ^@^ Ll.repeat (M.zero ()))))
end

module LazyM =
  Make(struct
        type 'a m = 'a Lazy.t
        let return x = lazy x
        let bind x f =
          lazy (Lazy.force (f (Lazy.force x)))
      end)

module LazyT(M : BatInterfaces.Monad) =
struct
  module M = Make(M)
  include Make(struct
                type 'a m = 'a Lazy.t M.m
                let return x = M.return (lazy x)
                let bind x f =
                  M.bind x (fun x ->
                            f (Lazy.force x))
              end)
  let lift x = M.lift1 (fun x -> lazy x) x
end

module List =
  MakePlus(struct
            type 'a m = 'a list
            let return x  = [x]
            let bind xs f = BatList.concat (List.map f xs)
            let zero () = []
            let plus xs ys = xs @ ys
            let null = function
                [] -> true
              | _  -> false
          end)

module ListT(M : BatInterfaces.Monad) =
struct
  module M = Make(M)
  include Make(struct
                type 'a m = 'a list M.m
                let return x  = M.return [x]
                let bind xs f =
                  M.bind xs (fun x ->
                             M.lift1 BatList.concat
                                     (M.map_a f x))
              end)
  let lift x = M.lift1 (fun x -> [x]) x
end

module LazyListM =
struct
  include MakeLazyPlus(struct
                        type 'a m = 'a LazyList.t
                        let return x  = Ll.singleton x
                        let bind xs f = Ll.concat (Ll.map f xs)
                        let zero () = Ll.nil
                        let rec lplus xs ys =
                          lazy (match Ll.next xs with
                                  Ll.Nil        -> Ll.next (Lazy.force ys)
                                | Ll.Cons(x,xs) -> Ll.Cons(x, lplus xs ys))
                        let null = Ll.is_empty
                      end)
end

module Option =
  MakePlus(struct
            type 'a m = 'a option
            let return x  = Some x
            let bind x f  = match x with
                None   -> None
              | Some y -> f y
            let zero ()   = None
            let plus x y  =
              match x,y with
                None, x        -> x
              | x, None        -> x
              | Some x, Some _ -> Some x
            let null      = BatOption.is_none
          end)

module OptionT(M : BatInterfaces.Monad) =
struct
  module M = Make(M)
  include Make(struct
                type 'a m = 'a option M.m
                let return x  = M.return (Some x)
                let bind xs f =
                      M.bind xs
                             (function
                                 None   -> M.return None
                               | Some x -> f x)
              end)
  let lift x = M.lift1 (fun x -> Some x) x
end

module Error(E : sig type e val defaultError : e end) =
struct
  type 'a err = Error  of E.e
              | Result of 'a
  include MakePlus(struct
                    type 'a m = 'a err
                    let return x = Result x
                    let bind x f = match x with
                        Error  e -> Error e
                      | Result x -> f x
                    let zero ()  = Error E.defaultError
                    let plus x y = match x with
                        Error  _ -> y
                      | Result x -> Result x
                    let null x   = match x with
                        Error _  -> true
                      | _        -> false
                  end)

  let throw e = Error e

  let catch x handler =
    match x with
      Error e  -> handler e
    | Result x -> return x

  let run_error err = err
end

module ErrorT(E : sig type e val defaultError : e end)(M : BatInterfaces.Monad) =
struct
  type 'a err = Error  of E.e
              | Result of 'a
  module M = Make(M)
  include MakePlus(struct
                    type 'a m = 'a err M.m
                    let return x = M.return (Result x)
                    let bind x f =
                      M.bind x (function
                                 | Result x -> f x
                                 | Error e  -> M.return (Error e))
                    let zero () = M.return (Error E.defaultError)
                    let plus x y =
                      M.bind x (function
                                 | Error _ -> y
                                 | x       -> M.return x)
                    let null _ = false
              end)

  let lift x  = M.lift1 (fun x -> Result x) x
  let throw e = M.return (Error e)
  let catch x handler =
    M.bind x (function
               | Error e -> handler e
               | x       -> M.return x)
  let run_error err = err
end

module Retry(E : sig
                   type e
                   type arg
                   type tag
                   val defaultError : e
                 end) =
struct
  type 'a err = Error  of (E.tag * (E.arg -> 'a err)) list * E.e
              | Result of 'a
  include MakePlus(
     struct
       type 'a m = 'a err
       let return x = Result x
       let rec bind x f = match x with
         | Result x -> f x
         | Error (retries,e) ->
            Error (BatList.map (fun (t,r) -> t,fun arg -> bind (r arg) f) retries,e)
       let zero ()  = Error ([],E.defaultError)
       let plus x y = match x with
           Error  _ -> y
         | Result x -> Result x
       let null x   = match x with
           Error _  -> true
         | _        -> false
     end)

  let add_retry tag retry = function
    | Error (retries,e) -> Error ((tag,retry)::retries,e)
    | x                 -> x
  let throw e = Error ([],e)

  let catch x handler =
    match x with
    | Error (_,e) -> handler e
    | Result x    -> return x

  let run_retry err = err
end

module Continuation(T : sig type r end) = struct
  include Make(struct
                type 'a m = ('a -> T.r) -> T.r

                let return x k = k x
                let bind c f k =
                  c (fun x -> (f x) k)
              end)
  let callCC kk k =
    kk (fun x -> fun _ -> k x) k
end


module State(T : sig type s end) =
struct
  include Make(struct
                type 'a m = T.s -> (T.s * 'a)
                let return x s  = (s, x)
                let bind xf f s =
                  let s',x = xf s in
                  f x s'
              end)
  let read s    = (s,s)
  let write x _ = (x,())
  let run x s   = x s
  let eval x s  = snd (x s)
  let modify f  = bind read (fun s -> write (f s))
end

module StateT(T : sig type s end)(M : BatInterfaces.Monad) =
struct
  module M = Make(M)
  include Make(struct
                type 'a m = T.s -> (T.s * 'a) M.m
                let return x s  = M.return (s, x)
                let bind xf f s =
                  M.bind (xf s) (fun (s',x) -> (f x) s')
              end)
  let read  s   = M.return (s,s)
  let write x _ = M.return (x,())
  let modify f  = bind read (fun s -> write (f s))
  let run   x s = x s
  let eval  x s = M.lift1 snd (x s)
  let lift x s  = M.lift1 (fun x -> (s,x)) x
end

module type Monoid =
sig
  type t
  val zero : unit -> t
  val plus : t -> t -> t
end

module Reader(M : sig type t end) = struct
  include Make(struct
                type 'a m = M.t -> 'a
                let return x _ = x
                let bind r f e =
                  f (r e) e
              end)
  let read     e = e
  let run e x    = x e
end

module Writer(M : Monoid) = struct
  include Make(struct
                type 'a m = M.t * 'a
                let return x     = M.zero (), x
                let bind (m,x) f =
                  let m',y = f x in
                  M.plus m m', y
              end)
  let listen (x,y) = (x,(x,y))
  let run (x,y)    = (x,y)
  let write x      = (x,())
end

module WriterT(Mon : Monoid)(M : BatInterfaces.Monad) =
struct
  module M = Make(M)
  module W = Writer(Mon)
  include Make(struct
                module WM = Make(W)
                type 'a m = 'a W.m M.m

                let return   x = M.return (W.return x)
                let bind x f =
                  M.bind x (fun x ->
                            let v,x = W.run x in
                            M.lift1 (WM.lift2
                                       (fun () y -> y)
                                       (W.write v))
                                    (f x))
              end)

  let listen x = M.lift1 W.listen x
  let write x = M.return (W.write x)
  let run xs = M.lift1 (fun x -> let (v,x) = W.run x in
                                 v,x) xs
  let lift x = M.lift1 W.return x
end

module CollectionOpt(C : BaseCollectionM) =
struct
  module Option = Make(Option)
  let liftp2 p x y = match x,y with
      None, _        -> true
    | Some _, None   -> false
    | Some x, Some y -> p x y

  include OptionT(C)
  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs

  let cmp_on p = liftp2 p

  let difference p = C.difference (cmp_on p)
  let unique ?(cmp = (=)) = C.unique ~cmp:(cmp_on cmp)
  let nub p = C.nub (cmp_on p)
  let maxima p = C.maxima (cmp_on p)
end

module CollectionWriter(Mon : sig
  include Monoid
  val cmp : t -> t -> bool
end)(C : BaseCollectionM) =
struct
  include WriterT(Mon)(C)
  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs

  let cmp_on p (vx,x) (vy,y) =
    p x y && Mon.cmp vx vy

  let difference p = C.difference (cmp_on p)
  let unique ?(cmp = (=)) = C.unique ~cmp:(cmp_on cmp)
  let maxima p = C.maxima (cmp_on p)
  let nub p = C.nub (cmp_on p)
end

module CollectionState(T :
  sig
    type s
    val cmp : s -> s -> bool
  end)(C : BaseCollectionM) =
struct
  include StateT(T)(C)
  let zero () _ = C.zero ()
  let lplus xs ys s = C.lplus (xs s) (lazy (Lazy.force ys s))
  let null _ = false

  let cmp_on p (s,x) (t,y) = p x y && T.cmp s t

  let difference p xs ys s = C.difference (cmp_on p) (xs s) (ys s)
  let unique ?(cmp = (=)) xs s = C.unique ~cmp:(cmp_on cmp) (xs s)
  let maxima p xs s = C.maxima (cmp_on p) (xs s)
  let nub p xs s = C.nub (cmp_on p) (xs s)

  let read s    = C.return (s,s)
  let write x _ = C.return (x,())
end
