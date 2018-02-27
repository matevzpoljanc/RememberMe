open Core_kernel
let memoize f =
    let table = Hashtbl.Poly.create () in 
    let g x =
        match Hashtbl.find table x with
        | Some y -> y
        | None -> let y = f x in 
            (Hashtbl.add_exn table ~key:x ~data:y; y)
            in
    g
;;

let memoize2 f =
    let table = Hashtbl.Poly.create () in 
    let g x0 x1 =
        match Hashtbl.find table (x0,x1) with
        | Some y -> y
        | None -> let y = f x0 x1 in 
            (Hashtbl.add_exn table ~key:(x0,x1) ~data:y; y)
            in
    g
;;

let memoize3 f =
    let table = Hashtbl.Poly.create () in 
    let g x0 x1 x2 =
        match Hashtbl.find table (x0,x1,x2) with
        | Some y -> y
        | None -> let y = f x0 x1 x2 in 
            (Hashtbl.add_exn table ~key:(x0,x1,x2) ~data:y; y)
            in
    g
;;

let memoize4 f =
    let table = Hashtbl.Poly.create () in 
    let g x0 x1 x2 x3 =
        match Hashtbl.find table (x0,x1,x2,x3) with
        | Some y -> y
        | None -> let y = f x0 x1 x2 x3 in 
            (Hashtbl.add_exn table ~key:(x0,x1,x2,x3) ~data:y; y)
            in
    g
;;