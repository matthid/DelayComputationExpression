F# computation library for computations which need to be in a special environment

Nuget: https://www.nuget.org/packages/DelayComputationExpression

## Example

Assume you use python interop (from https://github.com/pythonnet/pythonnet/).
In this case you need to retrieve the 'GIL' before you can call any interop call.
Instead of requesting the GIL everywhere we can use computation expressions to mark code as 'Needs the GIL'.
Just add this library and do the following:

```fsharp
module Python =
  type PythonDataMarker = class end 
  type PythonData<'T> = private { D : Delayed<'T> } with
    interface IDelayed<PythonDataMarker, 'T>
  let internal pythonConv =
    { new IDelayedConverter<PythonDataMarker> with
       member x.ToDelayed p = (p :?> PythonData<'a>).D
       member x.OfDelayed d = { D = d } :> IDelayed<PythonDataMarker, _> }
  let runInPython f = 
    use __ = Python.Runtime.Py.GIL()
    pythonConv.ToDelayed f |> Delayed.execute
  
  let python = ConcreteDelayedBuilder(pythonConv)
  let pythonSeq = DelayedSeqBuilder(python)

  // Build 'Safe' API on top
  let tf =
    python {
      // Call Python API
      let t = "test"
      return t
    }

  let seq =
    pythonSeq {
      for i in [1, 2, 3] do
        // Call Python API
        let! t = tf
        yield t + "test"
    }

  let first =
    seq
    |> pythonSeq.firstOrDefault "default"

  let run = first |> runInPython
```

Now callers can use your API, combine functions and then call 'runInPython' when they need the actual result in a safe manner.

Another use case would probably be executing stuff in the UI thread. But I didn't actually look into that.