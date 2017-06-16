// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Operator =
    | Op_Plus
    | Op_Min

type Value =
    | Val_B of bool     // Precisa separar em VTRUE e VFALSE
    | Val_N of int
    | Val_F of Value * Value
    | Val_Id

type Term =
    | Tm_N of int
    | Tm_B of bool
    | Tm_Op of Term * Operator * Term   // Operação
    | Tm_If of Term * Term * Term       // If
    | Tm_X of string                    // Identificador
    | Tm_Ap of Term * Term              // Aplicãção

let rec eval (t:Term) = // Term -> Value
    match t with

    // Number
    | Tm_N(t1) -> Val_N(t1)
    
    // Bool
    | Tm_B(true) -> Val_B(true)
    | Tm_B(false) -> Val_B(false)

    // Op + - * div == and or
    // ???

    // If
    | Tm_If (eval(t1), eval(t2), t3) when t1 == true -> t2
    | Tm_If (eval(t1), t2, eval(t3)) when t1 == false -> t3

    // Identificador
    // ???

    // Aplicação
    // ???

[<EntryPoint>]
let main argv = 
    printfn "%A" argv


    0 // return an integer exit code