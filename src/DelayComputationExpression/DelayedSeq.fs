namespace FSharpX.Computations

type IDelayedEnumerator<'b, 'T> =
  abstract MoveNext : unit -> IDelayed<'b, 'T option>
  inherit System.IDisposable

type IDelayedEnumerable<'b, 'T> =
  abstract GetEnumerator : unit -> IDelayedEnumerator<'b, 'T>

type DelayedSeq<'b, 'T> = IDelayedEnumerable<'b, 'T>

[<RequireQualifiedAccess>]
type internal AppendState<'b, 'T> =
    | NotStarted1     of DelayedSeq<'b, 'T> * DelayedSeq<'b, 'T>
    | HaveEnumerator1 of IDelayedEnumerator<'b, 'T> * DelayedSeq<'b, 'T>
    | NotStarted2     of DelayedSeq<'b, 'T>
    | HaveEnumerator2 of IDelayedEnumerator<'b, 'T> 
    | Finished 

[<RequireQualifiedAccess>]
type internal BindState<'b, 'T,'U> =
    | NotStarted of IDelayed<'b, 'T>
    | HaveEnumerator of IDelayedEnumerator<'b, 'U>
    | Finished

[<RequireQualifiedAccess>]
type internal TryWithState<'b, 'T> =
    | NotStarted of DelayedSeq<'b, 'T>
    | HaveBodyEnumerator of IDelayedEnumerator<'b, 'T>
    | HaveHandlerEnumerator of IDelayedEnumerator<'b, 'T>
    | Finished

[<RequireQualifiedAccess>]
type internal TryFinallyState<'b, 'T> =
    | NotStarted    of DelayedSeq<'b, 'T>
    | HaveBodyEnumerator of IDelayedEnumerator<'b, 'T>
    | Finished

[<RequireQualifiedAccess>]
type internal CollectState<'b, 'T,'U> =
    | NotStarted    of DelayedSeq<'b, 'T>
    | HaveInputEnumerator of IDelayedEnumerator<'b, 'T>
    | HaveInnerEnumerator of IDelayedEnumerator<'b, 'T> * IDelayedEnumerator<'b, 'U>
    | Finished

[<RequireQualifiedAccess>]
type internal CollectSeqState<'b, 'T,'U> =
    | NotStarted    of seq<'T>
    | HaveInputEnumerator of System.Collections.Generic.IEnumerator<'T>
    | HaveInnerEnumerator of System.Collections.Generic.IEnumerator<'T> * IDelayedEnumerator<'b, 'U>
    | Finished

[<RequireQualifiedAccess>]
type internal MapState<'T> =
    | NotStarted    of seq<'T>
    | HaveEnumerator of System.Collections.Generic.IEnumerator<'T>
    | Finished 
type DelayedSeqBuilder<'b>(builder : ConcreteDelayedBuilder<'b>) =
  let dispose (d:System.IDisposable) = match d with null -> () | _ -> d.Dispose()

  //[<GeneralizableValue>]
  member x.empty<'T> () : DelayedSeq<'b, 'T> = 
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = builder { return None }
                        member x.Dispose() = () } }
 
  member x.singleton (v:'T) : DelayedSeq<'b, 'T> = 
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  let state = ref 0
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = builder { let res = state.Value = 0
                                                        incr state; 
                                                        return (if res then Some v else None) }
                        member x.Dispose() = () } }
    
      

  member x.append (inp1: DelayedSeq<'b, 'T>) (inp2: DelayedSeq<'b, 'T>) : DelayedSeq<'b, 'T> =
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  let state = ref (AppendState.NotStarted1 (inp1, inp2) )
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | AppendState.NotStarted1 (inp1, inp2) -> 
                                        return! 
                                         (let enum1 = inp1.GetEnumerator()
                                          state := AppendState.HaveEnumerator1 (enum1, inp2)
                                          x.MoveNext())
                                    | AppendState.HaveEnumerator1 (enum1, inp2) ->
                                        let! res = enum1.MoveNext() 
                                        match res with 
                                        | None -> 
                                            return! 
                                              (state := AppendState.NotStarted2 inp2
                                               dispose enum1
                                               x.MoveNext())
                                        | Some _ -> 
                                            return res
                                    | AppendState.NotStarted2 inp2 -> 
                                        return! 
                                         (let enum2 = inp2.GetEnumerator()
                                          state := AppendState.HaveEnumerator2 enum2
                                          x.MoveNext())
                                    | AppendState.HaveEnumerator2 enum2 ->   
                                        let! res = enum2.MoveNext() 
                                        return (match res with
                                                | None -> 
                                                    state := AppendState.Finished
                                                    dispose enum2
                                                    None
                                                | Some _ -> 
                                                    res)
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | AppendState.HaveEnumerator1 (enum, _) 
                            | AppendState.HaveEnumerator2 enum -> 
                                state := AppendState.Finished
                                dispose enum 
                            | _ -> () } }


  member x.delay (f: unit -> DelayedSeq<'b, 'T>) : DelayedSeq<'b, 'T> = 
      { new IDelayedEnumerable<'b, 'T> with 
          member x.GetEnumerator() = f().GetEnumerator() }

  member x.bindAsync (f: 'T -> DelayedSeq<'b, 'U>) (inp : IDelayed<'b, 'T>) : DelayedSeq<'b, 'U> = 
        { new IDelayedEnumerable<'b, 'U> with 
              member x.GetEnumerator() = 
                  let state = ref (BindState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'U> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | BindState.NotStarted inp -> 
                                        let! v = inp 
                                        return! 
                                           (let s = f v
                                            let e = s.GetEnumerator()
                                            state := BindState.HaveEnumerator e
                                            x.MoveNext())
                                    | BindState.HaveEnumerator e ->   
                                        let! res = e.MoveNext() 
                                        return (match res with
                                                | None -> x.Dispose()
                                                | Some _ -> ()
                                                res)
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | BindState.HaveEnumerator e -> 
                                state := BindState.Finished
                                dispose e 
                            | _ -> () } }


  member x.Yield(v) = x.singleton v
  // This looks weird, but it is needed to allow:
  //
  //   while foo do
  //     do! something
  //
  // because F# translates body as Bind(something, fun () -> Return())
  member x.Return _ = x.empty ()
  member x.YieldFrom(s:DelayedSeq<'b, 'T>) = s
  member x.Zero () = x.empty ()
  member x.Bind (inp:IDelayed<'b, 'T>, body : 'T -> DelayedSeq<'b, 'U>) : DelayedSeq<'b, 'U> = x.bindAsync body inp
  member x.Combine (seq1:DelayedSeq<'b,'T>,seq2:DelayedSeq<'b,'T>) = x.append seq1 seq2
  member x.While (guard, body:DelayedSeq<'b,'T>) = 
    // Use F#'s support for Landin's knot for a low-allocation fixed-point
    let rec fix = x.delay (fun () -> if guard() then x.append body fix else x.empty())
    fix
  member x.Delay (f:unit -> DelayedSeq<'b,'T>) = 
    x.delay f
  
  //let delayedSeq = new PythonSeqBuilder()

  member x.emitEnumerator (ie: IDelayedEnumerator<'b, 'T>) = x {
      let! moven = ie.MoveNext() 
      let b = ref moven 
      while b.Value.IsSome do
          yield b.Value.Value 
          let! moven = ie.MoveNext() 
          b := moven }

  /// Implements the 'TryWith' functionality for computation builder
  member x.tryWith (inp: DelayedSeq<'b,'T>) (handler : exn -> DelayedSeq<'b,'T>) : DelayedSeq<'b,'T> = 
        // Note: this is put outside the object deliberately, so the object doesn't permanently capture inp1 and inp2
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  let state = ref (TryWithState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | TryWithState.NotStarted inp -> 
                                        let res = ref Unchecked.defaultof<_>
                                        try 
                                            res := Choice1Of2 (inp.GetEnumerator())
                                        with exn ->
                                            res := Choice2Of2 exn
                                        match res.Value with
                                        | Choice1Of2 r ->
                                            return! 
                                              (state := TryWithState.HaveBodyEnumerator r
                                               x.MoveNext())
                                        | Choice2Of2 exn -> 
                                            return! 
                                               (x.Dispose()
                                                let enum = (handler exn).GetEnumerator()
                                                state := TryWithState.HaveHandlerEnumerator enum
                                                x.MoveNext())
                                    | TryWithState.HaveBodyEnumerator e ->   
                                        let res = ref Unchecked.defaultof<_>
                                        try 
                                            let! r = e.MoveNext()
                                            res := Choice1Of2 r
                                        with exn -> 
                                            res := Choice2Of2 exn
                                        match res.Value with 
                                        | Choice1Of2 res -> 
                                            return 
                                                (match res with 
                                                 | None -> x.Dispose()
                                                 | _ -> ()
                                                 res)
                                        | Choice2Of2 exn -> 
                                            return! 
                                              (x.Dispose()
                                               let e = (handler exn).GetEnumerator()
                                               state := TryWithState.HaveHandlerEnumerator e
                                               x.MoveNext())
                                    | TryWithState.HaveHandlerEnumerator e ->   
                                        let! res = e.MoveNext() 
                                        return (match res with 
                                                | Some _ -> res
                                                | None -> x.Dispose(); None)
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | TryWithState.HaveBodyEnumerator e | TryWithState.HaveHandlerEnumerator e -> 
                                state := TryWithState.Finished
                                dispose e 
                            | _ -> () } }
 


  // This pushes the handler through all the async computations
  // The (synchronous) compensation is run when the Dispose() is called
  member x.tryFinally (inp: DelayedSeq<'b, 'T>) (compensation : unit -> unit) : DelayedSeq<'b, 'T> = 
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  let state = ref (TryFinallyState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | TryFinallyState.NotStarted inp -> 
                                        return! 
                                           (let e = inp.GetEnumerator()
                                            state := TryFinallyState.HaveBodyEnumerator e
                                            x.MoveNext())
                                    | TryFinallyState.HaveBodyEnumerator e ->   
                                        let! res = e.MoveNext() 
                                        return 
                                           (match res with 
                                            | None -> x.Dispose()
                                            | Some _ -> ()
                                            res)
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | TryFinallyState.HaveBodyEnumerator e-> 
                                state := TryFinallyState.Finished
                                dispose e 
                                compensation()
                            | _ -> () } }


  member x.collect (f: 'T -> DelayedSeq<'b, 'U>) (inp: DelayedSeq<'b, 'T>) : DelayedSeq<'b, 'U> = 
        { new IDelayedEnumerable<'b, 'U> with 
              member x.GetEnumerator() = 
                  let state = ref (CollectState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'U> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | CollectState.NotStarted inp -> 
                                        return! 
                                           (let e1 = inp.GetEnumerator()
                                            state := CollectState.HaveInputEnumerator e1
                                            x.MoveNext())
                                    | CollectState.HaveInputEnumerator e1 ->   
                                        let! res1 = e1.MoveNext() 
                                        return! 
                                           (match res1 with
                                            | Some v1 ->
                                                let e2 = (f v1).GetEnumerator()
                                                state := CollectState.HaveInnerEnumerator (e1, e2)
                                            | None -> 
                                                x.Dispose()
                                            x.MoveNext())
                                    | CollectState.HaveInnerEnumerator (e1, e2) ->   
                                        let! res2 = e2.MoveNext() 
                                        match res2 with 
                                        | None ->
                                            state := CollectState.HaveInputEnumerator e1
                                            dispose e2
                                            return! x.MoveNext()
                                        | Some _ -> 
                                            return res2
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | CollectState.HaveInputEnumerator e1 -> 
                                state := CollectState.Finished
                                dispose e1 
                            | CollectState.HaveInnerEnumerator (e1, e2) -> 
                                state := CollectState.Finished
                                dispose e2
                                dispose e1 
                            | _ -> () } }


  // Like collect, but the input is a sequence, where no bind is required on each step of the enumeration
  member x.collectSeq (f: 'T -> DelayedSeq<'b, 'U>) (inp: seq<'T>) : DelayedSeq<'b, 'U> = 
        { new IDelayedEnumerable<'b, 'U> with 
              member x.GetEnumerator() = 
                  let state = ref (CollectSeqState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'U> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | CollectSeqState.NotStarted inp -> 
                                        return! 
                                           (let e1 = inp.GetEnumerator()
                                            state := CollectSeqState.HaveInputEnumerator e1
                                            x.MoveNext())
                                    | CollectSeqState.HaveInputEnumerator e1 ->   
                                        return! 
                                          (if e1.MoveNext()  then 
                                               let e2 = (f e1.Current).GetEnumerator()
                                               state := CollectSeqState.HaveInnerEnumerator (e1, e2)
                                           else
                                               x.Dispose()
                                           x.MoveNext())
                                    | CollectSeqState.HaveInnerEnumerator (e1, e2)->   
                                        let! res2 = e2.MoveNext() 
                                        match res2 with 
                                        | None ->
                                            return! 
                                              (state := CollectSeqState.HaveInputEnumerator e1
                                               dispose e2
                                               x.MoveNext())
                                        | Some _ -> 
                                            return res2
                                    | _ -> return None}
                        member x.Dispose() = 
                            match !state with 
                            | CollectSeqState.HaveInputEnumerator e1 -> 
                                state := CollectSeqState.Finished
                                dispose e1 
                            | CollectSeqState.HaveInnerEnumerator (e1, e2) -> 
                                state := CollectSeqState.Finished
                                dispose e2
                                dispose e1
                                x.Dispose()
                            | _ -> () } }


  member x.ofSeq (inp: seq<'T>) : DelayedSeq<'b, 'T> = 
        { new IDelayedEnumerable<'b, 'T> with 
              member x.GetEnumerator() = 
                  let state = ref (MapState.NotStarted inp)
                  { new IDelayedEnumerator<'b, 'T> with 
                        member x.MoveNext() = 
                          builder { match !state with 
                                    | MapState.NotStarted inp -> 
                                        let e = inp.GetEnumerator()
                                        state := MapState.HaveEnumerator e
                                        return! x.MoveNext()
                                    | MapState.HaveEnumerator e ->   
                                        return 
                                            (if e.MoveNext()  then 
                                                 Some e.Current
                                             else 
                                                 x.Dispose()
                                                 None)
                                    | _ -> return None }
                        member x.Dispose() = 
                            match !state with 
                            | MapState.HaveEnumerator e -> 
                                state := MapState.Finished
                                dispose e 
                            | _ -> () } }

  member x.iteriDelayedData f (source : DelayedSeq<'b, _>) = 
      builder { 
          use ie = source.GetEnumerator()
          let count = ref 0
          let! move = ie.MoveNext()
          let b = ref move
          while b.Value.IsSome do
              do! f !count b.Value.Value
              let! moven = ie.MoveNext()
              do incr count
                 b := moven
      }

  
  member x.iterDelayedData (f: 'T -> IDelayed<'b, unit>) (inp: DelayedSeq<'b, 'T>) = 
      x.iteriDelayedData (fun i x -> f x) inp
  member x.iteri (f: int -> 'T -> unit) (inp: DelayedSeq<'b, 'T>) = 
      x.iteriDelayedData (fun i x -> builder.Return (f i x)) inp
  
  // Add additional methods to the 'current' computation builder

  member x.TryFinally (body: DelayedSeq<'b, 'T>, compensation) = 
    x.tryFinally body compensation   

  member x.TryWith (body: DelayedSeq<'b, _>, handler: (exn -> DelayedSeq<'b, _>)) = 
    x.tryWith body handler

  member x.Using (resource: 'T, binder: 'T -> DelayedSeq<'b, 'U>) = 
    x.tryFinally (binder resource) (fun () -> 
      if box resource <> null then dispose resource)

  member x.For (seq:seq<'T>, action:'T -> DelayedSeq<'b, 'TResult>) = 
    x.collectSeq action seq

  member x.For (seq:DelayedSeq<'b, 'T>, action:'T -> DelayedSeq<'b, 'TResult>) = 
    x.collect action seq

  member x.unfoldDelayedData (f:'State -> IDelayed<'b, ('T * 'State) option>) (s:'State) : DelayedSeq<'b, 'T> = 
    x {       
      let s = ref s
      let fin = ref false
      while not !fin do
        let! next = f !s
        match next with
        | None ->
          fin := true
        | Some (a,s') ->
          yield a
          s := s' }

  member x.replicateInfinite (v:'T) : DelayedSeq<'b, 'T> =    
    x { 
        while true do 
            yield v }

  member x.replicateInfiniteDelayedData (v:IDelayed<'b, 'T>) : DelayedSeq<'b, 'T> =
    x { 
        while true do 
            let! v = v
            yield v }

  member x.replicate (count:int) (v:'T) : DelayedSeq<'b, 'T> =    
    x { 
        for i in 1 .. count do 
           yield v }

  // --------------------------------------------------------------------------
  // Additional combinators (implemented as python/PythonSeq computations)

  member x.mapDelayedData f (source : DelayedSeq<'b, 'T>) : DelayedSeq<'b, 'TResult> = x {
    for itm in source do 
      let! v = f itm
      yield v }

  member x.mapiDelayedData f (source : DelayedSeq<'b,'T>) : DelayedSeq<'b,'TResult> = x {
    let i = ref 0L
    for itm in source do 
      let! v = f i.Value itm
      i := i.Value + 1L
      yield v }

  member x.chooseDelayedData f (source : DelayedSeq<'b, 'T>) : DelayedSeq<'b, 'R> = x {
    for itm in source do
      let! v = f itm
      match v with 
      | Some v -> yield v 
      | _ -> () }

  member x.filterDelayedData f (source : DelayedSeq<'b, 'T>) = x {
    for v in source do
      let! b = f v
      if b then yield v }

  member x.Builder = builder

[<AutoOpen>]
module DelayedSeqExtensions =
  // Add asynchronous for loop to the 'async' computation builder
  type ConcreteDelayedBuilder<'b> with
    member internal x.For (seq:DelayedSeq<'b, 'T>, action:'T -> IDelayed<'b, unit>) =
      let seqBuilder = DelayedSeqBuilder(x)
      seq |> seqBuilder.iterDelayedData action 

  type DelayedSeqBuilder<'b> with

    member x.tryLast (source : DelayedSeq<'b, 'T>) = x.Builder { 
        use ie = source.GetEnumerator() 
        let! v = ie.MoveNext()
        let b = ref v
        let res = ref None
        while b.Value.IsSome do
            res := b.Value
            let! moven = ie.MoveNext()
            b := moven
        return res.Value }

    member x.lastOrDefault def (source : DelayedSeq<'b, 'T>) = x.Builder { 
        let! v = x.tryLast source
        match v with
        | None -> return def
        | Some v -> return v }


    member x.tryFirst (source : DelayedSeq<'b, 'T>) = x.Builder {
        use ie = source.GetEnumerator() 
        let! v = ie.MoveNext()
        let b = ref v
        if b.Value.IsSome then 
            return b.Value
        else 
           return None }

    member x.firstOrDefault def (source : DelayedSeq<'b, 'T>) = x.Builder {
        let! v = x.tryFirst source
        match v with
        | None -> return def
        | Some v -> return v }

    member x.scanDelayedData f (state:'TState) (source : DelayedSeq<'b, 'T>) = x { 
          yield state 
          let z = ref state
          use ie = source.GetEnumerator() 
          let! moveRes0 = ie.MoveNext()
          let b = ref moveRes0
          while b.Value.IsSome do
            let! zNext = f z.Value b.Value.Value
            z := zNext
            yield z.Value
            let! moveResNext = ie.MoveNext()
            b := moveResNext }

    member x.pairwise (source : DelayedSeq<'b, 'T>) = x {
        use ie = source.GetEnumerator() 
        let! v = ie.MoveNext()
        let b = ref v
        let prev = ref None
        while b.Value.IsSome do
            let v = b.Value.Value
            match prev.Value with 
            | None -> ()
            | Some p -> yield (p, v)
            prev := Some v
            let! moven = ie.MoveNext()
            b := moven }

    member x.pickDelayedData (f:'T -> IDelayed<'b, 'U option>) (source:DelayedSeq<'b, 'T>) = x.Builder { 
        use ie = source.GetEnumerator() 
        let! v = ie.MoveNext()
        let b = ref v
        let res = ref None
        while b.Value.IsSome && not res.Value.IsSome do
            let! fv = f b.Value.Value
            match fv with 
            | None -> 
                let! moven = ie.MoveNext()
                b := moven
            | Some _ as r -> 
                res := r
        match res.Value with
        | Some _ -> return res.Value.Value
        | None -> return raise(System.Collections.Generic.KeyNotFoundException()) }

    member x.pick f (source:DelayedSeq<'b, 'T>) =
      x.pickDelayedData (f >> x.Builder.Return) source

    member x.tryPickDelayedData f (source : DelayedSeq<'b, 'T>) = x.Builder { 
        use ie = source.GetEnumerator() 
        let! v = ie.MoveNext()
        let b = ref v
        let res = ref None
        while b.Value.IsSome && not res.Value.IsSome do
            let! fv = f b.Value.Value
            match fv with 
            | None -> 
                let! moven = ie.MoveNext()
                b := moven
            | Some _ as r -> 
                res := r
        return res.Value }

    member x.tryPick f (source : DelayedSeq<'b, 'T>) = 
      x.tryPickDelayedData (f >> x.Builder.Return) source 

    member x.contains value (source : DelayedSeq<'b, 'T>) = 
      source |> x.tryPick (fun v -> if v = value then Some () else None) |> x.Builder.map Option.isSome

    member x.tryFind f (source : DelayedSeq<'b, 'T>) = 
      source |> x.tryPick (fun v -> if f v then Some v else None)

    member x.exists f (source : DelayedSeq<'b, 'T>) = 
      source |> x.tryFind f |> x.Builder.map Option.isSome

    member x.forall f (source : DelayedSeq<'b, 'T>) = 
      source |> x.exists (f >> not) |> x.Builder.map not

    member x.foldDelayedData f (state:'State) (source : DelayedSeq<'b, 'T>) = 
      source |> x.scanDelayedData f state |> x.lastOrDefault state

    member x.fold f (state:'State) (source : DelayedSeq<'b, 'T>) = 
      x.foldDelayedData (fun st v -> f st v |> x.Builder.Return) state source 

    member x.length (source : DelayedSeq<'b, 'T>) = 
      x.fold (fun st _ -> st + 1L) 0L source 

    member x.sum (source : DelayedSeq<'b, 'T>) : IDelayed<'b, 'T> = 
      (LanguagePrimitives.GenericZero, source) ||> x.fold (+)

    member x.scan f (state:'State) (source : DelayedSeq<'b, 'T>) = 
      x.scanDelayedData (fun st v -> f st v |> x.Builder.Return) state source 

    member x.unfold f (state:'State) = 
      x.unfoldDelayedData (f >> x.Builder.Return) state 

    member x.initInfiniteDelayedData f = 
      0L |> x.unfoldDelayedData (fun n -> 
        x.Builder { let! x = f n 
                    return Some (x,n+1L) }) 

    member x.initDelayedData (count:int64) f = 
      0L |> x.unfoldDelayedData (fun n -> 
          x.Builder { 
              if n >= count then return None 
              else 
                  let! x = f n 
                  return Some (x,n+1L) }) 


    member x.init count f  = 
      x.initDelayedData count (f >> x.Builder.Return) 

    member x.initInfinite f  = 
      x.initInfiniteDelayedData (f >> x.Builder.Return) 

    member x.mapi f (source : DelayedSeq<'b, 'T>) = 
      x.mapiDelayedData (fun i d -> f i d |> x.Builder.Return) source

    member x.map f (source : DelayedSeq<'b, 'T>) = 
      x.mapDelayedData (f >> x.Builder.Return) source

    member x.indexed (source : DelayedSeq<'b, 'T>) = 
      x.mapi (fun i x -> (i,x)) source 

    member x.iter f (source : DelayedSeq<'b, 'T>) = 
      x.iterDelayedData (f >> x.Builder.Return) source

    member x.choose f (source : DelayedSeq<'b, 'T>) = 
      x.chooseDelayedData (f >> x.Builder.Return) source

    member x.filter f (source : DelayedSeq<'b, 'T>) =
      x.filterDelayedData (f >> x.Builder.Return) source
  
  let delayedSeq = new DelayedSeqBuilder<_>(delayed)