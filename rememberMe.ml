type backend = 
    | LocalHashTbl
    | GlobalHashTbl 
    | IrminFs
    | IrminMem
    

let memoize b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize
    | IrminFs -> let open Memoize_irmin_fs in memoize
    | IrminMem -> let open Memoize_irmin_mem in memoize

let memoize2 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize2
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize2
    | IrminFs -> let open Memoize_irmin_fs in memoize2
    | IrminMem -> let open Memoize_irmin_mem in memoize2

let memoize3 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize3
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize3
    | IrminFs -> let open Memoize_irmin_fs in memoize3
    | IrminMem -> let open Memoize_irmin_mem in memoize3

let memoize4 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize4
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize4
    | IrminFs -> let open Memoize_irmin_fs in memoize4
    | IrminMem -> let open Memoize_irmin_mem in memoize4