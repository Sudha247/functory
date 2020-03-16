(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for OCaml                   *)
(*  Copyright (C) 2010- Jean-Christophe Filliatre and Kalyan Krishnamani  *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Control

(* main loop: assigns tasks to workers, until no more task *)

let ncores = ref 1
let set_number_of_cores n = ncores := n

let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
let workers () = listij [] 1 !ncores

(*** using local files ***************************************************)
let rec slice b e l =
  match l with
  | [] -> []
  | h :: t ->
     let tail = if e=0 then [] else slice (b-1) (e-1) t in
     if b > 0 then tail else h :: tail

let helper = Sequential.compute

let compute ~worker ~master l =
    let len = List.length l in
    let inc = (len / (!ncores)) - 1 in

    let rec spawn nd l st incr =
        if (nd = 0) then []
        else Domain.spawn (fun _ -> helper ~worker ~master (slice st (st + inc) l))
        :: (spawn (nd - 1) l (st + inc + 1) inc)
    in let domains = spawn !ncores l 0 inc in
    List.iter Domain.join domains

(* derived API *)

include Map_fold.Make(struct let compute = compute end)
