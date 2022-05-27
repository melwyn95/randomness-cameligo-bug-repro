#import "storage.mligo" "Storage"
#import "parameter.mligo" "Parameter"
#import "errors.mligo" "Errors"


type storage = Storage.Types.t
type parameter = Parameter.Types.t
type return = operation list * storage

// Sender reveals its chest content
let reveal(_, s : Parameter.Types.reveal_param * storage) : operation list * storage =
    let committed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt s.secrets with
        | None -> acc && false
        | Some _x -> acc && true
    in
    let _all_chests_committed = Set.fold committed s.participants true in
    let revealed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt (Map.empty : (address, bool) map) with
        | None -> acc && false
        | Some _x -> acc && true
    in
    let all_chests_revealed = Set.fold revealed s.participants true in
    ([], s)
        
let main(ep, store : parameter * storage) : return =
    match ep with 
    | Commit(_) -> [],store
    | Reveal(p) -> reveal(p, store)
    | Reset(_) -> [],store
