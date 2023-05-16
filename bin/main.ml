let () = CCRandom.self_init ()

module Matrix = struct
  type t = float array array

  let pp fmt (x : t) =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.fprintf fmt "@[%a@]@." (Array.pp ~pp_sep (Array.pp Float.pp)) x

  let times (x : t) (y : t) =
    let num_rows_a = Array.length x in
    let num_cols_a = Array.length x.(0) in
    let num_cols_b = Array.length y.(0) in
    let c = Array.make_matrix num_rows_a num_cols_b 0.0 in
    for i = 0 to num_rows_a - 1 do
      for j = 0 to num_cols_b - 1 do
        let sum = ref 0.0 in
        for k = 0 to num_cols_a - 1 do
          sum := !sum +. (x.(i).(k) *. y.(k).(j))
        done;
        c.(i).(j) <- !sum
      done
    done;
    c

  let transpose m : t =
    let rows = Array.length m in
    let cols = Array.length m.(0) in
    let t = Array.make_matrix cols rows 0. in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        t.(j).(i) <- m.(i).(j)
      done
    done;
    t
  let ( * ) = times

  let equal x y =
    let rows_equal = Array.equal Float.equal in
    Array.equal rows_equal x y
end

module State = struct
  type t = float array array

  let random n : t =
    let rs = Random.get_state () in
    let random _ = Random.float 1.0 rs in
    Array.init n (fun _ -> Array.init 1 random)

  let pp fmt (x : t) = Format.fprintf fmt "@[%a@]@." Matrix.pp x
end

module Transition_probability = struct
  type t = Matrix.t

  let pp = Matrix.pp

  let random n : t =
    let rs = Random.get_state () in
    let random _ = Random.float 1.0 rs in
    let generate_row _ =
      let res = Array.init n random in
      let sum = Array.fold_left Float.add 0.0 res in
      let normalized = Array.map (fun x -> x /. sum) res in
      normalized
    in
    Array.init n generate_row
end

module Emission_probability = struct
  type t = Matrix.t

  let pp = Matrix.pp

  let random n k : t =
    let rs = Random.get_state () in
    let random _ = Random.float 1.0 rs in
    let generate_row _ =
      let res = Array.init k random in
      let sum = Array.fold_left Float.add 0.0 res in
      let normalized = Array.map (fun x -> x /. sum) res in
      normalized
    in
    Array.init n generate_row
end

let () =
  let init_prob = State.random 5 in
  let trans_prob = Transition_probability.random 5 in
  let emission_probs = Emission_probability.random 5 3 in
  Format.printf "@.@[init_prob: %a@]@." Matrix.pp init_prob;
  Format.printf "@[trans_prob:@.%a@]@." Matrix.pp trans_prob;
  let state = ref init_prob in
  for _ = 0 to 10 do
    let next_state = Matrix.times trans_prob !state in
    state := next_state;
    Format.printf "@[next state: %a@]@." Matrix.pp next_state;
    let transposed_state = Matrix.transpose next_state in
    let observation = Matrix.times transposed_state emission_probs in
    Format.printf "@[observation: %a@]@." Matrix.pp observation
  done
