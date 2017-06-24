// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

////////////////
// ESTRUTURAS //
////////////////

type Variable = string

type Operator =
    | Sum
    | Diff
    | Mult
    | Div
    | Eq
    | Neq
    | Ls
    | Lse
    | Gr
    | Gre
    | And
    | Or

type Tipo =
    | TyInt
    | TyBool
    | TyFn of Tipo * Tipo
    | TyUnmatched
and
    Typenv = (Variable * Tipo) list


type Expr =
    | Num of int
    | Bool of bool
    | Bop of Expr * Operator * Expr
    | If of Expr * Expr * Expr
    | Var of Variable
    | App of Expr * Expr
    | Lam of Variable * Tipo * Expr
    | Let of Variable * Tipo * Expr * Expr
    | Lrec of Variable * (Tipo * Tipo) * (Variable * Tipo * Expr) * Expr

type Value =
    | Vnum of int
    | Vbool of bool
    | Vclos of Variable * Expr * Env
    | Vrclos of Variable * Variable * Expr * Env
    | VRaise
and
    Env = (Variable * Value) list

/////////////
// FUNÇÕES //
/////////////

(** verifica se é valor *)
let rec isvalue (env:Env, e : Expr) = 

    match e with
        | Num(_) -> true
        | Bool(_) -> true
        | Bop(a,_,b) when isvalue(env, a) && isvalue(env, b) -> true
        | If(e1, e2, e3) when isvalue(env, e1) && isvalue(env, e2) && isvalue(env, e3) -> true
        | Var(_) -> true
        | App(_,_) -> true
        | Lam(_,_,_) -> true
        | Let(_,_,_,_) -> true
        | Lrec(_,(_,_),(_,_,_),_) -> true
        | _ -> false

let rec findvariable (env:Env, var:Variable) =

    match env with
        | (v, va)::rst when v = var -> va
        | (_, _)::tl -> findvariable(tl, var)
        | [] -> VRaise

let rec findvariabletype (env:Typenv, var:Variable) =

    match env with
        | (v, ty)::rst when v = var -> ty
        | (_, _)::tl -> findvariabletype(tl, var)
        | [] -> TyUnmatched






let rec eval (env:Env, t:Expr) = // Expr -> Value
    match t with

    // Number
    | Num(t1) -> Vnum(t1)
    
    // Bool
    | Bool(x) -> Vbool(x)

    // Op + - * div == and or
    | Bop (t1, op, t2) -> match (eval(env, t1), op, eval(env, t2)) with

                            | (Vnum(a), Sum, Vnum(b)) -> Vnum(a + b)
                            | (Vnum(a), Diff, Vnum(b)) -> Vnum(a - b)
                            | (Vnum(a), Mult, Vnum(b)) -> Vnum(a * b)
                            | (Vnum(a), Div, Vnum(0)) -> VRaise
                            | (Vnum(a), Div, Vnum(b)) -> Vnum(a / b)
                            | (Vnum(a), Ls, Vnum(b)) -> Vbool(a < b)
                            | (Vnum(a), Lse, Vnum(b)) -> Vbool(a <= b)
                            | (Vnum(a), Gr, Vnum(b)) -> Vbool(a > b)
                            | (Vnum(a), Gre, Vnum(b)) -> Vbool(a >= b)
                            | (Vnum(a), Neq, Vnum(b)) -> Vbool(a <> b)
                            | (Vnum(a), Eq, Vnum(b)) -> Vbool(a = b)

                            | (Vbool(a), Neq, Vbool(b)) -> Vbool(a <> b)
                            | (Vbool(a), Eq, Vbool(b)) -> Vbool(a = b)
                            | (Vbool(a), And, Vbool(b)) -> Vbool(a && b)
                            | (Vbool(a), Or, Vbool(b)) -> Vbool(a || b)

                            | _ -> VRaise

    // Identificador
    | Var (x) -> findvariable(env, x)

    // If
    | If (t1, t2, t3) when eval(env, t1) = Vbool(true) -> eval(env, t2)
    | If (t1, t2, t3) when eval(env, t1) = Vbool(false) -> eval(env, t3)
    
    // Fn
    | Lam (var, ty, t) -> Vclos(var, t, env)
    
    // Let                       env |- e1 ⇓ v'         {x → v'} + env |- e2 ⇓ v                            v
    | Let (x, ty, e1, e2) when (isvalue (env, e1) && (isvalue ((x, eval(env, e1))::env, e2))) -> eval((x, eval(env, e1))::env, e2)

    // Let rec                                      {f → <f, x, e1, env>} + env |- e2 ⇓ v                   v
    | Lrec (f, (ty1, ty2), (x, ty3, e1), e2) when (isvalue ((f, Vrclos(f, x, e1, env))::env, e2)) -> eval((f, Vrclos(f, x, e1, env))::env, e2)

    // App              env |- e1 ⇓ <x, e, env'>                env |- e2 ⇓ v'                  {x → v'} + env |- e ⇓ v                 v
    | App (e1, e2) -> match eval(env, e1) with
                        | Vclos(x, e, env') when                (isvalue (env, e2)) &&          isvalue ((x, eval(env, e2))::env', e) -> eval ((x, (eval (env, e2)))::env', e)
                        | Vrclos(f, x, e, env') when            (isvalue (env, e2)) &&          isvalue (((x, eval(env, e2))::(f, Vrclos(f, x, e, env'))::env'), e) -> eval (((x, eval(env, e2))::(f, Vrclos(f, x, e, env'))::env'), e)
                        | _ -> VRaise
    // App rec          env |- e1 ⇓ <f, x, e, env`>             env |- e2 ⇓ v'                  {x → v'} + {f → <f, x, e, env'>} + env' |- e ⇓ v                        v
         
    // No matching -> RAISE
    | _ -> VRaise 



(** verifica se a expressão é de dado tipo *)
let rec typecheck (env:Typenv, t : Expr) =

    match t with
        | Num(x) -> TyInt
        | Bool(x) -> TyBool
        | If(t1, t2, t3) when typecheck (env, t1) = TyBool && (typecheck (env, t2) = typecheck (env, t3)) -> typecheck (env, t2)
        | Bop (t1, op, t2) -> match (typecheck(env, t1), op, typecheck(env, t2)) with
                            | (TyInt, Sum, TyInt) -> TyInt
                            | (TyInt, Diff, TyInt) -> TyInt
                            | (TyInt, Mult, TyInt) -> TyInt
                            | (TyInt, Div, TyInt) -> TyInt
                            | (TyInt, Ls, TyInt) -> TyBool
                            | (TyInt, Lse, TyInt) -> TyBool
                            | (TyInt, Gr, TyInt) -> TyBool
                            | (TyInt, Gre, TyInt) -> TyBool
                            | (TyInt, Neq, TyInt) -> TyBool
                            | (TyInt, Eq, TyInt) -> TyBool

                            | (TyBool, Neq, TyBool) -> TyBool
                            | (TyBool, Eq, TyBool) -> TyBool
                            | (TyBool, And, TyBool) -> TyBool
                            | (TyBool, Or, TyBool) -> TyBool

                            | _ -> TyUnmatched
        | Var(x) -> findvariabletype(env, x)
        | Lam (var, ty, t) when ty <> TyUnmatched && typecheck((var, ty)::env, t) <> TyUnmatched -> TyFn(ty, typecheck((var, ty)::env, t))
        | Let (x, ty, e1, e2) when ty <> TyUnmatched && typecheck(env, e1) = ty && typecheck((x, ty)::env, e2) <> TyUnmatched -> typecheck((x, ty)::env, e2)
        | Lrec (f, (ty1, ty2), (x, ty3, e1), e2) when ty1 <> TyUnmatched && ty2 <> TyUnmatched && ty3 <> TyUnmatched
            && typecheck((f, TyFn(ty1, ty2))::(x, ty3)::env, e1) = ty2
            && typecheck((f, TyFn(ty1, ty2))::env, e2) <> TyUnmatched
            -> typecheck((f, TyFn(ty1, ty2))::env, e2)
        | App (e1, e2) -> match typecheck(env, e1) with
                        | TyFn(ty1, ty2) when typecheck(env, e2) = ty1 -> ty2

                        | _ -> TyUnmatched

        | _ -> TyUnmatched


/////////////
// STRINGS //
/////////////

(** transforma um operador em string **)
let rec optostring(op:Operator) =
    match op with
    | Sum -> " + "
    | Diff -> " - "
    | Mult -> " * "
    | Div -> " / "
    | Eq -> " = "
    | Neq -> " <> "
    | Ls -> " < "
    | Lse -> " <= "
    | Gr -> " > "
    | Gre -> " >= "
    | And -> " && "
    | Or -> " || "

(** transforma um tipo em string **)
let rec typetostring(ty:Tipo) =
    match ty with
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyFn(t1, t2) -> "fn " + typetostring(t1) + " -> " + typetostring(t2)

(** transforma uma expressão em string **)
let rec exprtostringhelper(t:Expr, spaces:string) =

    match t with
    | Num(a) -> string a
    | Bool(true) -> "true"
    | Bool(false) -> "false"
    | Bop(e1, op, e2) -> exprtostringhelper(e1, spaces) + optostring(op) + exprtostringhelper(e2, spaces)
    | If(e1, e2, e3) -> "if(" + exprtostringhelper(e1, spaces) + ") then " + exprtostringhelper(e2, spaces) + " else " + exprtostringhelper(e3, spaces)
    | Var(v) -> v
    | App(e1, e2) -> exprtostringhelper(e1, spaces) + " (" + exprtostringhelper(e2, spaces) + ")"
    | Lam(v, ty, e) -> "fn " + v + ": " + typetostring(ty) + " => " + exprtostringhelper(e, spaces + "  ")
    | Let(v, ty, e1, e2) ->  "let " + v + ": " + typetostring(ty) + " " + exprtostringhelper(e1, spaces) + " = " + exprtostringhelper(e2, spaces)
    | Lrec(v1, (ty1, ty2), (v2, ty3, e1), e2) -> "let rec " + v1 + ": " + typetostring(ty1) + " -> " + typetostring(ty2) + " = (\n" + spaces + "  " + "fn " + v2 + ": " + typetostring(ty3) + " => " + exprtostringhelper(e1, spaces + "  ") + "\n) \nin " + exprtostringhelper(e2, spaces)

(** helper da função que transforma um environment em string **)
let rec exprtostring(t:Expr) =
    
    exprtostringhelper(t, "")

(** transforma um valor em string **)
let rec valuetostring(v:Value) =
    
    match v with
    | Vnum(a) -> string a
    | Vbool(true) -> "true"
    | Vbool(false) -> "false"
    | Vclos(v, e, env) -> "<" + v + ", " + exprtostring(e) + ", " + "env" + "> with env = {" + envtostring(env) + "}"
    | Vrclos(v1, v2, e, env) -> "<" + v1 + ", " + v2 + ", " + exprtostring(e) + ", " + "env" + "> with env = {" + envtostring(env) + "}"
    | VRaise -> "exception raised"
    | _ -> "invalid string value"
and
(** transforma um environment em string **)
    envtostring(env:Env) =
            match env with
            | (v, va)::tl -> "(" + v + ", " + valuetostring(va) + (if env.Length > 1 then ")," else ")") + envtostring(tl)
            | [] -> ""

////////////
// MACROS //
////////////

(** processa uma operação informando a descrição da mesma e o resultado **)
let rec processexpr(t:Expr) =
    let typenv = Typenv.Empty
    let typecheckedexp = typecheck(typenv, t)
    if(typecheckedexp <> TyUnmatched) then
        let env = Env.Empty
        let result = valuetostring(eval (env, t))
        let desc = exprtostring (t)
        printfn "%A\n" desc
        printfn ">> %A\n" result
        printfn "-------------------------------------------------------\n\n"
    else
        printfn "Typecheck Error!\n"

//////////
// MAIN //
//////////

[<EntryPoint>]
let main argv = 
    
    let env = Env.Empty

    // Fatorial de 5 de acordo com o exemplo no moodle
    let factorial = Lrec("fat", (TyInt, TyInt) ,("x", TyInt, 
                            If(Bop(Var("x"), Eq, Num(0)),
                                Num(1),
                                Bop(Var("x"), Mult, App(Var("fat"), Bop(Var("x"), Diff, Num(1)))))
                            ), 
                            App(Var("fat"), Num(5)))

    (** IF *) 
    let term3a = Bop(Num(30), Gr, Num(20))
    let term3b = Bool(false)
    let term3c = Bop(Num(10), Diff, Num(100)) // TYPECHECK PRECISA DETECTAR ERRO AQUI!
    let term3 = If(term3a, term3b, term3c) 

    (** APP *)
    let term9b = (Bop(Var("x"), Div, Var("x")))
    let term10 = Lam(("x"), TyInt, term9b)
    let term11 = App(term10, Num(30))

    (** LET *)
    let term14a = Bop(Num(10), Mult, Num(5))
    let term14b = Bop(Var("x"), Div, Num(2))
    let term14c = Lam("x", TyInt, term14b)
    let term14 = Let("x", TyInt, term14a, term14c)

    // Teste
    processexpr(term3)
    processexpr(term11)
    processexpr(term14)
    processexpr(factorial)
    
    // Não fechao terminal até apertar 'enter'
    System.Console.ReadKey() |> ignore

    0 // return an integer exit code