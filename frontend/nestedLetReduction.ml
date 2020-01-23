(*Reduction of nested let expressions*)
let rec reduction expr =   (**)
     match expr with
     (*first case was copied from mincaml article*)
     | Knorm.Let(xt, e1, e2) ->
         let rec insert = function
           | Knorm.Let(yt, e3, e4) -> Knorm.Let(yt, e3, insert e4)
           | Knorm.LetRec(fundefs, e) -> Knorm.LetRec(fundefs, insert e)
           | Knorm.LetTuple(yts, z, e) -> Knorm.LetTuple(yts, z, insert e)
           | e -> Knorm.Let(xt, e, reduction e2) in
         insert (reduction e1)
     (*For recursive functions, reduce function body and let_body*)
     | Knorm.LetRec ({ name; args; body }, e) -> Knorm.LetRec ({ name; args; body = reduction body }, reduction e)
     (*Reduce the body of all if-else statements -e1 and e2*)
     | Knorm.IfEq ((x, y), e2, e3) -> Knorm.IfEq ((x, y), reduction e2, reduction e3)
     | Knorm.IfLe ((x, y), e2, e3) -> Knorm.IfLe ((x, y), reduction e2, reduction e3)
     (*Reduce the body of LetTuple*)
     | Knorm.LetTuple (vars, def, body) -> Knorm.LetTuple (vars, reduction def, reduction body)
     (*all other expressions that can't contain nested-lets should be returned*)
     | _ -> expr
