namespace Library
open System
open System.Text
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end
type ToBigInt =
    static member ToBigInt (x: sbyte     ) = bigint (int x)
    static member ToBigInt (x: int16     ) = bigint (int x)
    static member ToBigInt (x: int32     ) = bigint      x
    static member ToBigInt (x: int64     ) = bigint      x
    static member ToBigInt (x: nativeint ) = bigint (int x)
    static member ToBigInt (x: byte      ) = bigint (int x)
    static member ToBigInt (x: uint16    ) = bigint (int x)
    static member ToBigInt (x: unativeint) = bigint (int x)
    static member ToBigInt (x: bigint    ) =             x
    static member ToBigInt (x: uint32    ) = bigint      x
    static member ToBigInt (x: uint64    ) = bigint      x

    static member inline Invoke (x: 'Integral) : bigint =
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member ToBigInt : _ -> _) b)
        call_2 (Unchecked.defaultof<ToBigInt>, x)
type FromBigInt =
    inherit Default1
    //static member inline FromBigInt (_: ^R        , _: Default4  ) = fun (x: bigint) -> Explicit.Invoke x         : ^R
    //static member inline FromBigInt (_: ^R        , _: Default3  ) = fun (x: bigint) -> Implicit.Invoke (int64 x) : ^R
    //static member inline FromBigInt (_: ^R        , _: Default2  ) = fun (x: bigint) -> Implicit.Invoke x         : ^R
    static member inline FromBigInt (_: ^R        , _: Default1  ) = fun (x: bigint) -> (^R : (static member FromBigInt : _ -> ^R) x)
    static member inline FromBigInt (_: Default1  , _: Default1  ) = fun (x: bigint) -> (^R : (static member FromBigInt : _ -> ^R) x)
    static member        FromBigInt (_: int32     , _: FromBigInt) = fun (x: bigint) -> int             x
    static member        FromBigInt (_: int64     , _: FromBigInt) = fun (x: bigint) -> int64           x
    static member        FromBigInt (_: nativeint , _: FromBigInt) = fun (x: bigint) -> nativeint  (int x)
    static member        FromBigInt (_: unativeint, _: FromBigInt) = fun (x: bigint) -> unativeint (int x)
    static member        FromBigInt (_: bigint    , _: FromBigInt) = fun (x: bigint) ->                 x
    static member        FromBigInt (_: float     , _: FromBigInt) = fun (x: bigint) -> float           x
    static member        FromBigInt (_: sbyte     , _: FromBigInt) = fun (x: bigint) -> sbyte           x
    static member        FromBigInt (_: int16     , _: FromBigInt) = fun (x: bigint) -> int16           x
    static member        FromBigInt (_: byte      , _: FromBigInt) = fun (x: bigint) -> byte            x
    static member        FromBigInt (_: uint16    , _: FromBigInt) = fun (x: bigint) -> uint16          x
    static member        FromBigInt (_: uint32    , _: FromBigInt) = fun (x: bigint) -> uint32          x
    static member        FromBigInt (_: uint64    , _: FromBigInt) = fun (x: bigint) -> uint64          x
    static member        FromBigInt (_: float32   , _: FromBigInt) = fun (x: bigint) -> float32         x
    static member        FromBigInt (_: decimal   , _: FromBigInt) = fun (x: bigint) -> decimal         x

    static member inline Invoke (x: bigint) : 'Num =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member FromBigInt : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<FromBigInt> x
type Zero =
    inherit Default1

    static member        Zero (_: System.TimeSpan                , _: Zero    ) = System.TimeSpan ()
    //static member        Zero (_: DmStruct                       , _: Zero    ) = Unchecked.defaultof<DmStruct>
    static member        Zero (_: list<'a>                       , _: Zero    ) = []   :   list<'a>
    static member        Zero (_: option<'a>                     , _: Zero    ) = None : option<'a>
    static member        Zero (_: array<'a>                      , _: Zero    ) = [||] :  array<'a>
    static member        Zero (_: string                         , _: Zero    ) = ""
    static member        Zero (_: StringBuilder                  , _: Zero    ) = new StringBuilder ()
    static member        Zero (_: unit                           , _: Zero    ) = ()
    static member        Zero (_: bool                           , _: Zero    ) = false
    static member        Zero (_: Set<'a>                        , _: Zero    ) = Set.empty : Set<'a>
    static member        Zero (_: Map<'a,'b>                     , _: Zero    ) = Map.empty : Map<'a,'b>

    static member inline Invoke () = 
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Zero : _*_ -> _) b, a)
        let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r
        call Unchecked.defaultof<Zero>
    

type Zero with
    static member inline Zero (_: Task<'a>, _: Zero) =
        let (v: 'a) = Zero.Invoke ()
        let s = TaskCompletionSource ()
        s.SetResult v
        s.Task

    static member inline Zero (_: 'T->'Monoid               , _: Zero) = (fun _ -> Zero.Invoke ()) : 'T->'Monoid
    static member inline Zero (_: Async<'a>                 , _: Zero) = let (v: 'a) = Zero.Invoke () in async.Return v
    static member inline Zero (_: Expr<'a>                  , _: Zero) = let (v: 'a) = Zero.Invoke () in Expr.Cast<'a>(Expr.Value (v))
    static member inline Zero (_: Lazy<'a>                  , _: Zero) = let (v: 'a) = Zero.Invoke () in lazy v
    static member        Zero (_: Dictionary<'a,'b>         , _: Zero) = Dictionary<'a,'b> ()
    static member        Zero (_: ResizeArray<'a>           , _: Zero) = ResizeArray () : ResizeArray<'a>

type Zero with
    //static member inline Zero (_: ^R                             , _: Default6) = FromInt64.Invoke 0L : ^R

//    static member inline Zero (_: ^R                             , _: Default5) = Implicit.Invoke 0   : ^R

    static member        Zero (_: seq<'a>                        , _: Default4) = Seq.empty      : seq<'a>
//    static member        Zero (_: IEnumerator<'a>                , _: Default4) = FSharpPlus.Enumerator.Empty () : IEnumerator<'a>
    static member        Zero (_: IDictionary<'a,'b>             , _: Default4) = Dictionary<'a,'b> () :> IDictionary<'a,'b>
    static member        Zero (_: IReadOnlyDictionary<'a,'b>     , _: Default4) = Dictionary<'a,'b> () :> IReadOnlyDictionary<'a,'b>

    static member inline Zero (_: 't                             , _: Default3) = (^t : (static member Empty: ^t) ()) : 't

//    static member inline Zero (_: 't                             , _: Default2) = FromInt32.InvokeOnInstance 0        : 't
    static member inline Zero (_: ^t when ^t: null and ^t: struct, _: Default2) = id

    static member inline Zero (_: 't                             , _: Default1) = LanguagePrimitives.GenericZero : 't
    static member inline Zero (_: ^t when ^t: null and ^t: struct, _: Default1) = id




module Rational =
    let inline numerator   x = (^F : (member Numerator   : 'R) x)
    let inline denominator x = (^F : (member Denominator : 'R) x)
type Sqrt=
    inherit Default1

    static member inline Invoke (x: 'Integral) : 'Integral =
        let inline call_2 (t: ^t, a: ^a) = ((^t or ^a) : (static member Sqrt : _*_ -> _) a, t)
        call_2 (Unchecked.defaultof<Sqrt>, x)
        
type Sqrt with
    static member inline Sqrt (x: 'Rational, _: Sqrt) =
        if x < Zero.Invoke () then failwith "exnSqrtOfNegative"
        else 
            let (n: 'i, d: 'i) = Rational.numerator x, Rational.denominator x
            let toRational (x: 'i) = (ToBigInt.Invoke >> FromBigInt.Invoke) x : 'Rational
            let n, d = Sqrt.Invoke n, Sqrt.Invoke d
            (toRational n / toRational d)
            
module Say =
    let hello name =
        printfn "Hello %s" name
