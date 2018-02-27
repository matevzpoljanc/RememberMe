open RememberMe

let config = Irmin_git.config ~root:"db1" ()

let add2 x = x+2
let m_add2 = memoize (IrminFs config) (fun x -> add2 x)
let m_add = memoize2 (IrminFs config) (fun x y -> x+y)

let () =
    print_endline @@ string_of_int @@ m_add 2 4;
    print_endline @@ string_of_int @@ m_add2 5;
    print_endline @@ string_of_int @@ m_add2 8;
