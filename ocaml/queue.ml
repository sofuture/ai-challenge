
type 'a queue = ('a list * 'a list) ref;;

module Queue = struct
    exception Empty

    let create () = ref ([], []);;

    let add queue x =
        let front, back = !queue in
        queue := (x::front, back);;

    let rec take queue =
        match !queue with
        | ([], []) ->
            raise Empty
        | (front, x::back) ->
            queue := (front, back); x
        | (front, []) ->
            queue := ([], List.rev front);
            take queue;;

    let length queue =
        let front, back = !queue in
        List.length front + List.length back;;

end;;

