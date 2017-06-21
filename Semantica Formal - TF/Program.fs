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


let rec eval (t:Expr) = // Expr -> Value
    match t with

    // Number
    | Num(t1) -> Vnum(t1)
    
    // Bool
    | Bool(x) -> Vbool(x)


    // Op + - * div == and or
    // | Bop (t1, op, t2) when op = Sum -> eval(t1) + eval(t2)


    // If
    | If (t1, t2, t3) when eval(t1) = Vbool(true) -> eval(t2)
    | If (t1, t2, t3) when eval(t1) = Vbool(false) -> eval(t3)

    // | Lam (var, ty, t) -> Vclos(var, t, Exp(var, t))

    // Identificador
    // ???

    // Aplicação
    // | App (t1, t2) -> eval(t1)(eval(t2))

[<EntryPoint>]
let main argv = 
    
    // Testes
    let a = eval (Bool(true))
    let c = If(Bool(false), Bool(false), Bool(true))
    let b = eval (If(c, Num(3), Num(4)))
    printfn "%A" b


    0 // return an integer exit code