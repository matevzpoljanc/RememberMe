let (>>=) = Lwt.bind
 

module MemoResult(AO:Irmin.AO_MAKER)(RW:Irmin.RW_MAKER): sig 
    type t

    val create: Irmin.config -> t Lwt.t
    
    val find_or_add: t -> 'a -> default:(unit -> 'b) -> 'b Lwt.t

end = struct
    module AO = AO(Irmin.Hash.SHA1)(Tc.String)
    exception VauleNotFoundInRW
    exception ValueNotFoundInAO
    exception FunctionNotAllowedToBeCalled
    
    module Key = struct
        include Tc.String
        let to_hum t = match Ezjsonm.decode_string (to_json t) with
            | Some s -> string_of_int @@ Hashtbl.hash s
            | None -> raise (Ezjsonm.Parse_error ((to_json t), "Invalid arguments" ))
        let of_hum s = raise FunctionNotAllowedToBeCalled
    end

    module RW = RW(Key)(Irmin.Hash.SHA1)

    type t = {table_rw: RW.t; table_ao: AO.t}
    let create config =
        AO.create config >>= fun ao_t ->
        RW.create config >>= fun rw_t ->
        Lwt.return {table_rw = rw_t; table_ao = ao_t}

    (* Use Marshal module to provide polymorphism keys and values *)
    let find t key = RW.read t.table_rw (Marshal.to_string key [Marshal.Closures]) >>= 
        function None -> raise VauleNotFoundInRW
            |(Some ao_key) -> (*Printf.printf "%s\n" @@ Irmin.Hash.SHA1.to_hum ao_key ;*) AO.read t.table_ao ao_key >>= 
                function None -> raise ValueNotFoundInAO
                    | Some v -> (*Printf.printf "Unmarshalled address Irmin: %d\n" (2 * Obj.magic v) ;*)Lwt.return (Marshal.from_string v 0)

    let insert t key value = AO.add t.table_ao (Marshal.to_string value [Marshal.Closures]) >>= fun k ->
        RW.update t.table_rw (Marshal.to_string key [Marshal.Closures]) k
    
    let find_or_add t key ~default = 
        let s_key = Marshal.to_string key [Marshal.Closures] in 
        RW.read t.table_rw s_key >>=
        function None -> 
            let value =  default () in
             insert t key value >>= fun () -> find t key
        | Some _ -> find t key
end