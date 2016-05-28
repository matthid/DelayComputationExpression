namespace FSharpX.Computations

/// First type paramter is only to make two implementions incompatible with each other.
type IDelayed<'b, 'a> = interface end
type DefaultMarker = class end 
type Delayed<'a> =
  private { 
    Delayed : (unit -> 'a)
    mutable Cache : 'a option
  } with
  interface IDelayed<DefaultMarker, 'a>

/// Ideally we want 'b to be a type constructor and return 'b<'a>...
type IDelayedConverter<'b> =
  abstract ToDelayed : IDelayed<'b, 'a> -> Delayed<'a>
  abstract OfDelayed : Delayed<'a> -> IDelayed<'b, 'a>

module Delayed =
  let create f = { Delayed = f; Cache = None }
  let execute (f:Delayed<_>) =
    match f.Cache with
    | Some d -> d
    | None ->
      let res = f.Delayed()
      f.Cache <- Some res
      res
      
  let map f d =
    (fun () -> f (d |> execute)) |> create

  let conv =
    { new IDelayedConverter<DefaultMarker> with
       member x.ToDelayed p = (p :?> Delayed<'a>)
       member x.OfDelayed d = d :> IDelayed<DefaultMarker, _> }

//let runInPython f = 
//  use __ = Python.Runtime.Py.GIL()
//  f |> getPythonData

type ConcreteDelayedBuilder<'b>(conv : IDelayedConverter<'b>) =
    let execute a = a |> conv.ToDelayed |> Delayed.execute
    let create f = f |> Delayed.create |> conv.OfDelayed
      
    member x.Bind(d, f) =
        (fun () -> 
          let r = d |> execute
          f r |> execute
          ) |> create

    member x.Return(d) = 
        (fun () -> d) |> create
    member x.ReturnFrom (d) = d
    member x.Delay (f) = 
      (fun () -> f() |> execute) |> create
    member x.Combine (v, next) = x.Bind(v, fun () -> next)
    member x.Run (f) = f
    member x.Zero () = (fun () -> ()) |> create
    member x.TryWith (d, recover) =
      (fun () -> 
        try
          d |> execute
        with e -> recover e |> execute) |> create
    member x.TryFinally (d, final) =
      (fun () -> 
        try
          d |> execute
        finally final ()) |> create
    member x.While (condF, body) =
      (fun () -> 
        while condF() do
          body |> execute) |> create
    member x.Using (var, block) =
      (fun () -> 
        use v = var
        block v |> execute) |> create
    member x.For (seq, action) = 
      (fun () -> 
        for item in seq do
          action item |> execute) |> create
          
    member x.map f d =
        Delayed.map f (conv.ToDelayed d) |> conv.OfDelayed

  


[<AutoOpen>]
module DelayedExtensions =
  
  let delayed = ConcreteDelayedBuilder(Delayed.conv)

