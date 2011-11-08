type map = float array array;;

let p m =
    for i = 0 to (Array.length m) - 1 do
        for j = 0 to (Array.length m.(i)) - 1 do
            Printf.printf "%3d " (int_of_float m.(i).(j))
        done;
        Printf.printf "\n"
    done;;

let rec diffuse mdat frontier explored =
    match frontier with
    | [] -> mdat
    | _ ->
        mdat;;

let m = Array.make_matrix 50 50 0.0 in
let poi = [(10,20); (33,40)] in
m.(10).(20) <- 500.0;
m.(33).(40) <- 400.0;
List.fold_left (fun acc x -> diffuse acc [x] (Hashtbl.create 20)) m poi;;

