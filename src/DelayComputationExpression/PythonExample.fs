namespace Test.Python.Interop

open FSharpX.Computations

module Python =
  type PythonDataMarker = class end 
  type PythonData<'T> = private { D : Delayed<'T> } with
    interface IDelayed<PythonDataMarker, 'T>
  let internal pythonConv =
    { new IDelayedConverter<PythonDataMarker> with
       member x.ToDelayed p = (p :?> PythonData<'a>).D
       member x.OfDelayed d = { D = d } :> IDelayed<PythonDataMarker, _> }
  let runInPython f = 
    //use __ = Python.Runtime.Py.GIL()
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