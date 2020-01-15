(*Reduction of nested let expressions*)
let rec reduction expr =   (**)
     match expr with
     (*first case was copied from mincaml article*)
     | Knorm.Let(xt, e1, e2) ->
         let rec insert = function
           | Knorm.Let(yt, e3, e4) -> Knorm.Let(yt, e3, insert e4)
           | Knorm.LetRec(fundefs, e) -> Knorm.LetRec(fundefs, insert e)
           | Knorm.LetTuple(yts, z, e) -> Knorm.LetTuple(yts, z, insert e)
           | e -> Let(xt, e, reduction e2) in (*write e as Knorm.e? *)
         insert (reduction e1)
     | Knorm.LetRec ({ name; args; body }, e) -> Knorm.LetRec ({ name; args; body = reduction body }, reduction e)
     | Knorm.IfEq ((x, y), e2, e3) -> Knorm.IfEq ((x, y), reduction e2, reduction e3)
     | Knorm.IfLe ((x, y), e2, e3) -> Knorm.IfLe ((x, y), reduction e2, reduction e3)
     | Knorm.LetTuple (vars, def, body) -> Knorm.LetTuple (vars, def, reduction body)
     |__ -> expr
