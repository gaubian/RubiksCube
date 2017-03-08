(**
c_o[i] = orientation change of corner initially at i_th position
e_p[i] = new position of edge initially at i_th position
c_p[i] = new position of corner initially at i_th position
**)
type ll = {co : int array; ep : int array; cp : int array}

module LL =
struct
   type t = ll
   let compare = Pervasives.compare
end

module Pair (OrdOne : Set.OrderedType) (OrdTwo : Set.OrderedType) =
struct
   type t = (OrdOne.t * OrdTwo.t)
   let compare a b =
      match OrdOne.compare (fst a) (fst b) with
      | 0 -> OrdTwo.compare (snd a) (snd b)
      | x -> x
end

module LLSet = Set.Make(LL)

module LLPairMap = Map.Make(Pair(LL)(LL))

let ll_map f set =
   LLSet.(fold (fun x -> add (f x)) set empty)

let create_from a b c = {co = a; ep = b; cp = c}

let ($) stat alg =
   let open Array in
   let ans = create_from
      (mapi (fun i x -> (x + alg.co.(stat.cp.(i))) mod 3) stat.co)
      [|0;1;2;3|] [|0;1;2;3|]
   in
      iteri (fun i j -> ans.ep.(i) <- alg.ep.(j)) stat.ep;
      iteri (fun i j -> ans.cp.(i) <- alg.cp.(j)) stat.cp;
      ans

let id = create_from [|0;0;0;0|] [|0;1;2;3|] [|0;1;2;3|]

let up = create_from [|0;0;0;0|] [|3;0;1;2|] [|3;0;1;2|]

let up_two = up $ up

let up_inv = up_two $ up

let inv alg =
   let x = ref alg and y = ref (alg $ alg) in
      while !y <> id do x := !y; y := !x $ alg done;
      !x

let sym alg =
   let sym_cp i = 3 - i in
   let sym_ep = function 1 -> 3 | 3 -> 1 | x -> x in
   let sym_op x = (3 - x) mod 3 in
   let ans = create_from [|0;0;0;0|] [|0;1;2;3|] [|0;1;2;3|] in
      for i = 0 to 3 do
         ans.co.(sym_cp i) <- sym_op alg.co.(i);
         ans.cp.(sym_cp i) <- sym_cp alg.cp.(i);
         ans.ep.(sym_ep i) <- sym_ep alg.ep.(i)
      done;
      ans

let add_f f set =
   LLSet.(fold (fun x s -> add x (add (f x) s)) set empty)

let normalize alg =
   alg
   |> LLSet.singleton
   |> add_f inv
   |> add_f sym
   |> add_f (($) up)
   |> add_f (($) up)
   |> add_f (($) up)
   |> add_f (fun x -> x $ up)
   |> add_f (fun x -> x $ up)
   |> add_f (fun x -> x $ up)
   |> LLSet.min_elt

let epsilon x =
   let ans = ref @@ -1 in
      for i = 0 to 2 do
      for j = i+1 to 3 do
         if x.(i) > x.(j) then ans := - !ans
      done;
      done;
      !ans

let all_pos =
   let l = ref [] in
   let recu s x =
      if epsilon x.ep = epsilon x.cp
      then LLSet.add (normalize x) s
      else s
   in
      for c_pa = 0 to 3 do
      for c_pb = 0 to 3 do
         if c_pb <> c_pa then
      for c_pc = 0 to 3 do
         if c_pc <> c_pa && c_pc <> c_pb then
      let c_pd = 6 - c_pa - c_pb - c_pc in
      for e_pa = 0 to 3 do
      for e_pb = 0 to 3 do
         if e_pb <> e_pa then
      for c_oa = 0 to 2 do
      for c_ob = 0 to 2 do
      for c_oc = 0 to 2 do
      for e_pc = 0 to 3 do
         if e_pc <> e_pa && e_pc <> e_pb then
         let e_pd = 6 - e_pa - e_pb - e_pc in
         let co = [|c_oa; c_ob; c_oc; (6 - c_oa - c_ob - c_oc) mod 3|] in
         let cp = [|c_pa; c_pb; c_pc; c_pd|] in
         let ep = [|e_pa; e_pb; e_pc; e_pd|] in
         let x = create_from co ep cp in
            l := x :: !l
      done;
      done;
      done;
      done;
      done;
      done;
      done;
      done;
      done;
      List.fold_left recu LLSet.empty !l

let from a b =
   a
   |> LLSet.singleton
   |> add_f inv
   |> add_f sym
   |> add_f (($) up)
   |> add_f (($) up)
   |> add_f (($) up)
   |> List.(fold_right add_f (map ($) [b; inv b; sym b; sym (inv b)]))
   |> ll_map normalize

let map_compose =
   let b x y = LLPairMap.add (x,y) (from x y) in
   let a x = LLSet.fold (b x) all_pos in
      LLSet.fold a all_pos LLPairMap.empty

let _ =
   print_int (LLSet.cardinal all_pos)
