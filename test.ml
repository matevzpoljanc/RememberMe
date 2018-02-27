open RememberMe

let add2 x = x+2
let m_add2 = memoize IrminFs (fun x -> add2 x)
let m_add = memoize2 IrminFs (fun x y -> x+y)

let () =
    print_endline @@ string_of_int @@ m_add 2 4;
    print_endline @@ string_of_int @@ m_add2 5
