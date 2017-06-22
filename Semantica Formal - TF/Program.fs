// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Variable = String

type Operator =
    | Sum
    | Diff
    | Mult
    | Div
    | Eq
    | Leq

type Tipo =
    | TyInt
    | TyBool
    | TyFn of Tipo * Tipo

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
and
    Env = (Variable * Value) list

(** verifica se é valor *)
let rec isvalue (env:Env, e : Expr) = 

    match e with
    | Num(_) -> true
    | Bool(_) -> true
    | Bop(_,_,_) -> true 
    | If(_,_,_)-> true
    | Var(_) -> true
    | App(_,_) -> true
    | Lam(_,_,_) -> true
    | Let(_,_,_,_) -> true
    | Lrec(_,(_,_),(_,_,_),_) -> true
    | _ -> false


let rec eval (env:Env, t:Expr) = // Expr -> Value
    match t with

    // Number
    | Num(t1) -> Vnum(t1)
    
    // Bool
    | Bool(x) -> Vbool(x)

    // Op + - * div == and or
    // | Bop (t1, op, t2) when op = Sum -> eval(t1) + eval(t2)

    // Identificador
    // ???

    // If
    | If (t1, t2, t3) when eval(env, t1) = Vbool(true) -> eval(env, t2)
    | If (t1, t2, t3) when eval(env, t1) = Vbool(false) -> eval(env, t3)
    
    // Fn
    | Lam (var, ty, t) -> Vclos(var, t, env)
    
    // Let                       env |- e1 ⇓ v'         {x → v'} + env |- e2 ⇓ v                            v
    | Let (x, ty, e1, e2) when (isvalue (env, e1) && (isvalue ((x, eval(env, e1))::env, e2))) -> eval((x, eval(env, e1))::env, e2)

    // Let rec                                      {f → <f, x, e1, env>} + env |- e2 ⇓ v                   v
    | Lrec (f, (ty1, ty2), (x, ty3, e1), e2) when (isvalue ((f, Vrclos(f, x, e1, env))::env, e2)) -> eval((f, Vrclos(f, x, e1, env))::env, e2)

    // App              env |- e1 ⇓ <x, e, env'>                env |- e2 ⇓ v'              {x → v'} + env |- e ⇓ v
    //| App (e1, e2) when eval (env, e1) = Vclos(x, e, env') && (isvalue (env, e2)) && isvalue ((x, eval(env, e2))::env, e) -> eval ((x, (isvalue e2))::env, e)

    // App rec           env |- e1 ⇓ <f, x, e, env`>        env |- e2 ⇓ v'        {x → v'} + {f → <f, x, e, env'>} + env' |- e ⇓ v
    //| App (e1, e2) when eval(e1) = Vrclos(f, x,  ->

[<EntryPoint>]
let main argv = 
    
    let env = Env.Empty

    // Testes
    let a = eval (env, Bool(true))
    let c = If(Bool(false), Bool(false), Bool(true))
    let b = eval (env, If(c, Num(3), Num(4)))
    printfn "%A" b


    0 // return an integer exit code