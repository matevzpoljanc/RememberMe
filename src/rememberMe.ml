type backend = 
    | LocalHashTbl
    | GlobalHashTbl 
    | IrminFs: Irmin.config -> backend
    | IrminFsOnly: Irmin.config -> backend
    | IrminMem
    
let fs = ref None
let fs_only = ref None
let memoize b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize
    | IrminFs config -> let open Memoize_irmin_fs in (
        match !fs with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs :=  Some m ; memoize m
        | Some m -> memoize m
        )
    | IrminFsOnly config -> let open Memoize_irmin_fs_only in (
        match !fs_only with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs_only :=  Some m ; memoize m
        | Some m -> memoize m
    )
    | IrminMem -> let open Memoize_irmin_mem in memoize

let memoize2 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize2
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize2
    | IrminFs config -> let open Memoize_irmin_fs in (
        match !fs with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs :=  Some m ; memoize2 m
        | Some m -> memoize2 m
        )
    | IrminFsOnly config -> let open Memoize_irmin_fs_only in (
        match !fs_only with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs_only :=  Some m ; memoize2 m
        | Some m -> memoize2 m
    )
    | IrminMem -> let open Memoize_irmin_mem in memoize2

let memoize3 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize3
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize3
    | IrminFs config -> let open Memoize_irmin_fs in (
        match !fs with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs :=  Some m ; memoize3 m
        | Some m -> memoize3 m
        )
    | IrminFsOnly config -> let open Memoize_irmin_fs_only in (
        match !fs_only with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs_only :=  Some m ; memoize3 m
        | Some m -> memoize3 m
    )
    | IrminMem -> let open Memoize_irmin_mem in memoize3

let memoize4 b =
    match b with
    | LocalHashTbl -> let open Memoize_local_hashtbl in memoize4
    | GlobalHashTbl -> let open Memoize_global_hashtbl in memoize4
    | IrminFs config -> let open Memoize_irmin_fs in (
        match !fs with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs :=  Some m ; memoize4 m
        | Some m -> memoize4 m
        )
    | IrminFsOnly config -> let open Memoize_irmin_fs_only in (
        match !fs_only with 
        | None -> 
            let m = Lwt_main.run (IrminHashTbl.create @@ config) in 
            fs_only :=  Some m ; memoize4 m
        | Some m -> memoize4 m
    )
    | IrminMem -> let open Memoize_irmin_mem in memoize4