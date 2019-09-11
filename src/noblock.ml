type fd = Unix.file_descr

type out_update =
  | AddWrite of int * fd * string
  | RemoveWrite of int

type in_update =
  | AddRead of int * fd
  | RemoveRead of int

type s = (int * (int * string) list) * (int * int list)
type w = in_update list * out_update list

let zero_w = [],[]
let plus_w (inps,outs) (inps',outs')
  = inps @ inps', outs @ outs'

let with_w w (x,s,w') = x,s,plus_w w w'

type 'a prog =
  | Return   of 'a
  | TryLater of 'a t
 and 'a t = s -> 'a prog * s * w

let rec map_async f x s =
  match x s with
  | Return x,s,w   -> Return (f x),s,w
  | TryLater x,s,w -> TryLater (map_async f x),s,w

let return x s = Return x, s, zero_w

let rec (<*>) f x s =
  match f s with
  | Return f,s,w   -> with_w w (map_async ((@@) f) x s)
  | TryLater f,s,w -> match x s with
                         | Return x,s,w'   ->
                            TryLater (map_async ((|>) x) f),s,plus_w w w'
                         | TryLater x,s,w' ->
                            TryLater (fun _ -> (f <*> x) s),s,plus_w w w'

let rec (>>=) x f s =
  match x s with
  | Return x,s,w   -> with_w w (f x s)
  | TryLater x,s,w -> TryLater (fun _ -> (x >>= f) s),s,w

module App = Applicative.Make(
                 struct
                   type 'a m = 'a t
                   let return = return
                   let (<*>) = (<*>)
                 end)

include Monad.Make(struct
                    type 'a m = 'a t
                    let return = return
                    let bind = (>>=)
                  end)

include (App : Applicative.Applicative with type 'a m := 'a m)

let read_line fd =
  let rec read = function
    | (pos,(pos',line)::inps), outs when pos = pos'
      -> Return line, ((pos+1,inps),outs), ([RemoveRead pos],[])
    | (_,inps), outs
      -> TryLater read, ((0,inps),outs), zero_w in
  fun ((pos,_),_ as s) -> TryLater read, s, ([AddRead (pos,fd)],[])

let write_line fd str =
  let rec write = function
    | inps, (pos,pos'::outs) when pos = pos'
      -> Return (), (inps,(0,outs)), ([],[RemoveWrite pos])
    | inps, (pos,outs)
      -> TryLater write, (inps,(pos+1,outs)), zero_w in
  fun (_,(pos,_) as s) -> TryLater write, s, ([],[AddWrite (pos,fd,str)])
