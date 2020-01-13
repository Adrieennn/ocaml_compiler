let find mapping var = 
	List.assoc_opt var mapping

let replace_name mapping name =
	match find mapping var with
	| None -> name
	| Some(new_name) new_name



let rec convert exp mapping =
	match exp with
	| Knorm.Unit c | Knorm.Int c |Knorm.Float c -> c
	| Knorm.Add (v1,v2) -> 
		let c_v1 = replace_name mapping v1 in 
		let c_v2 = replace_name mapping v2 in
		Knorm.Add(c_v1,c_v2)
	| Knorm.Sub (v1,v2) ->
		let c_v1 = convert v1 mapping in 
		let c_v2 = convert v2 mapping in
		Knorm.Sub(c_v1,c_v2)
	| Knorm.FAdd (v1,v2) -> 
		let c_v1 = convert v1 mapping in 
		let c_v2 = convert v2 mapping in
		Knorm.FAdd(c_v1,c_v2)
	| Knorm.FSub (v1,v2) ->
		let c_v1 = convert v1 mapping in 
		let c_v2 = convert v2 mapping in
		Knorm.FSub(c_v1,c_v2)
	| Knorm.FMul (v1,v2) -> 
		let c_v1 = convert v1 mapping in 
		let c_v2 = convert v2 mapping in
		Knorm.FMul(c_v1,c_v2)
	| Knorm.FDiv (v1,v2) ->
		let c_v1 = convert v1 mapping in 
		let c_v2 = convert v2 mapping in
		Knorm.FDiv(c_v1,c_v2)
	| Knorm.Let ((id, typ), def, body) ->
		let new_mapping = ( match find mapping id with
		| None -> (id, id) :: mapping
		| Some(_) -> let suffix = id.genid in (id, id ^ suffix) :: mapping
		) in
		Knorm.Let(((id ^ suffix), typ), convert def new_env, convert body new_env)
	| Knorm.Var x ->
		(match find mapping x with
		| None -> Knorm.Var x
		| Some(new_x) -> Knorm.Var new_x 
	) 
	| Knorm.LetRec ((id, typ), args ,body) ->
	Knorm.LetRec(((replace_name id), typ), List.map (fun (id, t) -> (replace_name mapping id, t)) args, convert body mapping)
	| _ -> failwith "under construction"