


let rec effect e (l:'a list ref) = 
  match e with
  | Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Tuple _ | Knorm.Array _ | Knorm.Get _ | Knorm.Var _ -> false
  | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _ | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ -> false
  | Knorm.App (id,args) ->
  	(match List.find ( fun elm -> elm = id) !l with
  	| id -> true
  	| _ -> false)
  | Knorm.Let (var,def,body) ->
  	((effect def l) && (effect body l))
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body=fun_body }, let_body) ->
  	if (effect fun_body l) 
  	then 
  	((l:= fun_id :: !l) ; true)
  	else 
  	(effect let_body l)  
  | Knorm.LetTuple (vars, def, body) ->
  	(effect def l) && (effect body l)
  | Knorm.Put _ -> true
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      (effect e1 l) && (effect e2 l)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      (effect e1 l) && (effect e2 l)


let rec fvar (id : Id.t) (exp : Knorm.t) = 
	match exp with
	| Knorm.Unit | Knorm.Int _ | Knorm.Float _ |Knorm.Array _-> true
	| Knorm.Put (e1, e2, e3) -> (not (id=e1)) && (not (id=e2)) && (not (id=e3))
	| Knorm.Var i ->
		(match i with
		 | id -> false
		 | _ -> true) 
	| Knorm.Add (v1,v2) | Knorm.Sub (v1,v2) | Knorm.FAdd (v1,v2) | Knorm.FSub (v1,v2) | Knorm.FMul (v1,v2) | Knorm.FDiv (v1,v2) -> (not(id=v1) && not(id=v2))
	| Knorm.Tuple l -> 
		(List.for_all (fun b -> (b=true)) (List.map (fun i -> not(id=i)) l))
	| Knorm.Get (e1,e2) -> (not (id=e1)) && (not (id=e2))
	(*assume that after alpha conversion all variable name is different*)
	| Knorm.Let ((name, _t), e1, e2) -> ((fvar id e1) && (fvar id e2))
	| Knorm.LetRec ({name;args;body},let_body) -> ((fvar id body) && (fvar id let_body))
	| Knorm.LetTuple (l, e1, e2) -> ((fvar id e1) && (fvar id e2))
	| Knorm.IfEq ((v1, v2), e1, e2) | Knorm.IfLe ((v1, v2), e1, e2) -> ((not (id=v1)) && (not (id=v2)) && (fvar id e1) && (fvar id e2))
	| Knorm.App (f, args) -> (not (f=id)) && (List.for_all (fun b -> (b=true)) (List.map (fun i -> not(id=i)) args))


let rec elim exp l = 
match exp with
 | Knorm.Let ((id, _t), e1, e2) -> 
 	if ((not (effect e1 l)) && (fvar id e2)) 
 		then 
 			elim e2 l 
		else 
 			Knorm.Let ((id, _t), elim e1 l, (elim e2 l))
 | Knorm.LetRec ({ Knorm.name = fun_id, fun_typ; args; body = fun_body }, let_body) ->
 	if ((not (effect fun_body l)) && (fvar fun_id let_body)) 
 		then 
 			elim let_body l 
		else 
 			Knorm.LetRec({ name = (fun_id, fun_typ); args;  body=(elim fun_body l) }, (elim let_body l))
 | Knorm.LetTuple (vars, def, body) -> Knorm.LetTuple (vars, elim def l, elim body l)
 | Knorm.IfEq ((v1, v2), e1, e2) ->
 	Knorm.IfEq ((v1, v2), elim e1 l, elim e2 l)
 | Knorm.IfLe ((v1, v2), e1, e2) ->
 	Knorm.IfLe ((v1, v2), elim e1 l, elim e2 l)
 | (Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _ | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ | Knorm.Var _ | Knorm.App _ | Knorm.Tuple _ | Knorm.Array _ | Knorm.Get _ | Knorm.Put _) as c -> c

 let elimination exp = let l = ref [] in elim exp l