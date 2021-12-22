(* let spring rest acceleration friction =
  let mutable current = rest
  let mutable velocity = 0.

  let progress target =
      velocity <- velocity + (target - current) * acceleration
      velocity <- velocity * friction
      current  <- current + velocity
      current

  fun target ->
      seq {
          while abs (current - target) > 0.1 do
              yield progress target
      }

let simulate = spring 0. 0.8 0.06

for step in simulate 50. do
  printfn "Current %f" step
  Thread.Sleep 16

for step in simulate 0. do
  printfn "Current %f" step
  Thread.Sleep 16 *)

let spring rest acceleration friction =
  (fun t ->
  (* takes a target and redefines all the functions *)
  )
  let rec do_sim c v t =
    let progress current velocity target = 
      let v = velocity +. (target -. current) *. acceleration in
      let v = v *. friction in
      let c = current +. v in
      (c, v)
    in

    let rec advance current velocity target : float list =
      if abs_float ((current -. target)) > 0.1 then
        let new_c, new_v = progress current velocity target in
        [new_c] @ advance new_c new_v target
      else
        [current]
    in
    let steps = advance c v t in
    do_sim
  in
  do_sim rest 0.0

let _ =
  let simulate = spring 0.0 0.8 0.06 in
  let steps : float list = simulate 50. in
  List.iter (fun step ->
    Printf.printf "Step: %f\n" step
  ) steps;

  let steps : float list = simulate 0. in
  List.iter (fun step ->
    Printf.printf "Step: %f\n" step
  ) steps

  (* Kick off the simulation *)
