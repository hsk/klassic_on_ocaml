open Value

let show = Value.show_env

let variables = function
  | Environment(v,_) -> v

let rec apply(key: string)(env:environment): value =
  match env with
  | Environment(variables, parent) ->
    try
      List.assoc key !variables
    with
    | Not_found ->
      match parent with
      | Some env -> apply key env
      | None -> failwith(Printf.sprintf "symbol'%s' not found" key)

let rec change k v list = match list with
  | [] -> raise Not_found
  | (k1,_)::xs when k = k1 -> (k,v)::xs
  | x::xs -> x::change k v xs

let rec set((key: string),(value: value))(Environment(variables, parent): environment): value =
  try
    variables := change key value !variables;
    value
  with
    | Not_found ->
      match parent with
      | None -> value
      | Some(env) -> set (key,value) env


let update((key: string), (value: value))(Environment(variables, parent): environment): value =
  let rec update k v list = match list with
    | [] -> [k,v]
    | (k1,_)::xs when k = k1 -> (k,v)::xs
    | x::xs -> x::update k v xs
  in
  variables := update key value !variables;
  value


let define((name: string), (body: value list -> value))(env:environment): value = 
  update(name, NativeFunctionValue(body)) env
