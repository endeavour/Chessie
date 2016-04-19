/// Contains error propagation functions and a computation expression builder for Railway-oriented programming.
namespace Chessie.ErrorHandling

open System

/// Represents the result of a computation.
type Result<'TSuccess, 'TMessage, 'TRollback> = 
    /// Represents the result of a successful computation.
    | Ok of 'TSuccess * 'TMessage list * 'TRollback list
    /// Represents the result of a failed computation.
    | Bad of 'TMessage list * 'TRollback list

    /// Creates a Failure result with the given messages.
    static member FailWith(messages:'TMessage seq) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Bad(messages |> Seq.toList, [])

    /// Creates a Failure result with the given message.
    static member FailWith(message:'TMessage) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Bad([message], [])
    
    /// Creates a Success result with the given value.
    static member Succeed(value:'TSuccess) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,[],[])

    /// Creates a Success result with the given value and the given message.
    static member Succeed(value:'TSuccess,message:'TMessage) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,[message],[])

    /// Creates a Success result with the given value and the given message.
    static member Succeed(value:'TSuccess,messages:'TMessage seq) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,messages |> Seq.toList,[])

    /// Creates a Success result with the given value.
    static member Succeed(value:'TSuccess, rollback:'TRollback) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,[],[rollback])

    /// Creates a Success result with the given value and the given message.
    static member Succeed(value:'TSuccess,message:'TMessage, rollback:'TRollback) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,[message],[rollback])

    /// Creates a Success result with the given value and the given message.
    static member Succeed(value:'TSuccess,messages:'TMessage seq, rollback:'TRollback) : Result<'TSuccess, 'TMessage, 'TRollback> = Result<'TSuccess, 'TMessage, 'TRollback>.Ok(value,messages |> Seq.toList,[rollback])

    /// Executes the given function on a given success or captures the failure
    static member Try(func: Func<_>) : Result<'TSuccess,exn,'TRollback> =        
        try
            Ok(func.Invoke(),[],[])
        with
        | exn -> Bad ([exn], [])

    /// Executes the given function on a given success or captures the failure
    static member TryWithRollback(func: Func<_>, rollback:'TRollback) : Result<'TSuccess,exn,'TRollback> =        
        try
            Ok(func.Invoke(),[],[rollback])
        with
        | exn -> Bad ([exn], [])

    /// Converts the result into a string.
    override this.ToString() =
        match this with
        | Ok(v,msgs,_) -> sprintf "OK: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
        | Bad(msgs,_) -> sprintf "Error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))    

type SimpleResult<'a,'b> = Result<'a,'b,unit>

/// Basic combinators and operators for error handling.
[<AutoOpen>]
module Trial =  
    /// Wraps a value in a Success
    let inline ok<'TSuccess,'TMessage,'TRollback> (x:'TSuccess) : Result<'TSuccess,'TMessage,'TRollback> = Ok(x, [], [])
    let inline okWithRollback<'TSuccess,'TMessage,'TRollback> (rollback:'TRollback) (x:'TSuccess) : Result<'TSuccess,'TMessage,'TRollback> = Ok(x, [], [rollback])

    /// Wraps a value in a Success
    let inline pass<'TSuccess,'TMessage,'TRollback> (x:'TSuccess) : Result<'TSuccess,'TMessage,'TRollback> = Ok(x, [], [])
    let inline passWithRollback<'TSuccess,'TMessage,'TRollback> (rollback:'TRollback) (x:'TSuccess) : Result<'TSuccess,'TMessage,'TRollback> = Ok(x, [], [rollback])

    /// Wraps a value in a Success and adds a message
    let inline warn<'TSuccess,'TMessage,'TRollback> (msg:'TMessage) (x:'TSuccess) : Result<'TSuccess,'TMessage, 'TRollback> = Ok(x,[msg], [])
    let inline warnWithRollback<'TSuccess,'TMessage,'TRollback> (rollback:'TRollback) (msg:'TMessage) (x:'TSuccess) : Result<'TSuccess,'TMessage, 'TRollback> = Ok(x,[msg], [rollback])

    /// Wraps a message in a Failure
    let inline fail<'TSuccess,'Message,'TRollback> (msg:'Message) : Result<'TSuccess,'Message,'TRollback> = Bad([ msg ], [])
    let inline failWithRollback<'TSuccess,'Message,'TRollback> (rollback : 'TRollback) (msg:'Message) : Result<'TSuccess,'Message,'TRollback> = Bad([ msg ], [ rollback ])

    /// Executes the given function on a given success or captures the exception in a failure
    let inline Catch f x = Result<_,_,_>.Try(fun () -> f x)
    let inline CatchWithRollback f rollback x = Result<_,_,_>.TryWithRollback((fun () -> f x), rollback)

    /// Returns true if the result was not successful.
    let inline failed result = 
        match result with
        | Bad _ -> true
        | _ -> false

    /// Takes a Result and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    let inline either fSuccess fFailure trialResult = 
        match trialResult with
        | Ok(x, msgs, rollback) -> fSuccess (x, msgs, rollback)
        | Bad(msgs, rollback) -> fFailure (msgs, rollback)

    /// If the given result is a Success the wrapped value will be returned. 
    ///Otherwise the function throws an exception with Failure message of the result.
    let inline returnOrFail (result:Result<_,_,_>) = 
        let inline raiseExn msgs = 
            msgs
            |> Seq.map (sprintf "%O")
            |> String.concat (Environment.NewLine + "\t")
            |> failwith
        either (fun (r,_,_) -> r) (fun (m,_) -> raiseExn m) result

    /// Appends the given messages with the messages in the given result.
    let inline mergeMessages msgs result = 
        let inline fSuccess (x, msgs2, rollback) = Ok(x, msgs @ msgs2, rollback)
        let inline fFailure (errs,rollback) = Bad(errs @ msgs, rollback)
        either fSuccess fFailure result

    /// Appends the given messages with the messages in the given result.
    let inline mergeRollbacks rollbacks result = 
        let inline fSuccess (x, msgs, rollbacks2) = Ok(x, msgs, rollbacks @ rollbacks2)
        let inline fFailure (errs, rollbacks2) = Bad(errs, rollbacks @ rollbacks2)
        either fSuccess fFailure result

    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    let inline bind f result = 
        let inline fSuccess (x, msgs, rollbacks) = f x |> mergeMessages msgs |> mergeRollbacks rollbacks
        let inline fFailure (msgs) = Bad (msgs)
        either fSuccess fFailure result

   /// Flattens a nested result given the Failure types are equal
    let inline flatten (result : Result<Result<_,_,_>,_,_>) =
        result |> bind id

    /// If the result is a Success it executes the given function on the value. 
    /// Otherwise the exisiting failure is propagated.
    /// This is the infix operator version of ErrorHandling.bind
    let inline (>>=) result f = bind f result

    /// If the wrapped function is a success and the given result is a success the function is applied on the value. 
    /// Otherwise the exisiting error messages are propagated.
    let inline apply wrappedFunction result = 
        match wrappedFunction, result with
        | Ok(f, msgs1, rollbacks1), Ok(x, msgs2, rollbacks2) -> Ok(f x, msgs1 @ msgs2, rollbacks1 @ rollbacks2)
        | Bad (errs, rollbacks), Ok(_, _, _) -> Bad(errs, rollbacks)
        | Ok(_, _, _), Bad (errs, rollbacks) -> Bad(errs, rollbacks)
        | Bad (errs1, rollbacks1), Bad (errs2, rollbacks2) -> Bad(errs1 @ errs2, rollbacks1 @ rollbacks2)

    /// If the wrapped function is a success and the given result is a success the function is applied on the value. 
    /// Otherwise the existing error messages are propagated.
    /// This is the infix operator version of ErrorHandling.apply
    let inline (<*>) wrappedFunction result = apply wrappedFunction result

    /// Lifts a function into a Result container and applies it on the given result.
    let inline lift f result = apply (ok f) result

    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded but rollbacks will be conserved.
    let inline mapFailure f result =
        match result with
        | Ok (v,_,rollback) -> Ok(v,[],rollback)
        | Bad (errs, rollbacks) -> Bad (f errs, rollbacks)

    /// Lifts a function into a Result and applies it on the given result.
    /// This is the infix operator version of ErrorHandling.lift
    let inline (<!>) f result = lift f result

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = f <!> a <*> b

    /// If the result is a Success it executes the given success function on the value and the messages.
    /// If the result is a Failure it executes the given failure function on the messages.
    /// Result is propagated unchanged.
    let inline eitherTee fSuccess fFailure result =
        let inline tee f x = f x; x;
        tee (either fSuccess fFailure) result

    /// If the result is a Success it executes the given function on the value and the messages.
    /// Result is propagated unchanged.
    let inline successTee f result = 
        eitherTee f ignore result

    /// If the result is a Failure it executes the given function on the messages.
    /// Result is propagated unchanged.
    let inline failureTee f result = 
        eitherTee ignore f result

    /// Collects a sequence of Results and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    let inline collect xs = 
        Seq.fold (fun result next -> 
            match result, next with
            | Ok(vs, m1, r1), Ok(v, m2, r2) -> Ok(v :: vs, m1 @ m2, r1 @ r2)
            | Ok(_, m1, r1), Bad(m2, r2) | Bad(m1, r1), Ok(_, m2, r2) -> Bad(m1 @ m2, r1 @ r2)
            | Bad(m1, r1), Bad(m2, r2) -> Bad(m1 @ m2, r1 @ r2)) (ok []) xs
        |> lift List.rev

    /// Converts an option into a Result.
    let inline failIfNone message result = 
        match result with
        | Some x -> ok x
        | None -> fail message

    /// Converts a Choice into a Result.
    let inline ofChoice choice =
        match choice with
        | Choice1Of2 v -> ok v
        | Choice2Of2 v -> fail v

    /// Categorizes a result based on its state and the presence of extra messages
    let inline (|Pass|Warn|Fail|) result =
      match result with
      | Ok  (value, [], rs) -> Pass (value, rs)
      | Ok  (value, msgs, rs) -> Warn (value, msgs, rs)
      | Bad (msgs, rs)  -> Fail (msgs, rs)

    let inline failOnWarnings result =
      match result with
      | Warn (_,msgs,rs) -> Bad (msgs,rs)
      | _             -> result 

    let inline rollback (result:Result<_,_,unit->unit>) =
      match result with
      | Ok(_,_,rs) | Bad (_, rs) ->
        rs
        |> List.rev
        |> List.fold ((>>)) (ignore<unit>)

    let inline asyncRollback (result:Result<_,_,Async<unit>>) =
      match result with
      | Ok(_,_,rs) | Bad (_, rs) ->
        rs
        |> List.rev
        |> List.fold (fun acc n -> async.Combine(acc,n)) (async.Zero())

    /// Builder type for error handling computation expressions.
    type TrialBuilder() = 
        member __.Zero() = ok()
        member __.Bind(m, f) = bind f m
        member __.Return(x) = ok x
        member __.ReturnFrom(x) = x
        member __.Combine (a, b) = bind b a
        member __.Delay f = f
        member __.Run f = f ()
        member __.TryWith (body, handler) =
            try
                body()
            with
            | e -> handler e
        member __.TryFinally (body, compensation) =
            try
                body()
            finally
                compensation()
        member x.Using(d:#IDisposable, body) =
            let result = fun () -> body d
            x.TryFinally (result, fun () ->
                match d with
                | null -> ()
                | d -> d.Dispose())
        member x.While (guard, body) =
            if not <| guard () then
                x.Zero()
            else
                bind (fun () -> x.While(guard, body)) (body())
        member x.For(s:seq<_>, body) =
            x.Using(s.GetEnumerator(), fun enum ->
                x.While(enum.MoveNext,
                    x.Delay(fun () -> body enum.Current)))

    /// Wraps computations in an error handling computation expression.
    let trial = TrialBuilder()

/// Represents the result of an async computation
[<NoComparison;NoEquality>]
type AsyncResult<'a, 'b, 'c> = 
    | AR of Async<Result<'a, 'b, 'c>>

/// Useful functions for combining error handling computations with async computations.
[<AutoOpen>]
module AsyncExtensions = 
    /// Useful functions for combining error handling computations with async computations.
    [<RequireQualifiedAccess>]
    module Async = 
        /// Creates an async computation that return the given value
        let singleton value = value |> async.Return

        /// Creates an async computation that runs a computation and
        /// when it generates a result run a binding function on the said result
        let bind f x = async.Bind(x, f)

        /// Creates an async computation that runs a mapping function on the result of an async computation
        let map f x = x |> bind (f >> singleton)

        /// Creates an async computation from an asyncTrial computation
        let ofAsyncResult (AR x) = x

/// Basic support for async error handling computation
[<AutoOpen>]
module AsyncTrial = 
    /// Builder type for error handling in async computation expressions.
    type AsyncTrialBuilder() = 
        member __.Return value : AsyncResult<'value, 'msg, 'rollback> = 
            value
            |> ok
            |> Async.singleton
            |> AR
        
        member __.ReturnFrom(asyncResult : AsyncResult<'value, 'msg, 'rollback>) = asyncResult
        member this.Zero() : AsyncResult<unit, 'b, 'c> = this.Return()
        member __.Delay(generator : unit -> AsyncResult<'a, 'b, 'c>) : AsyncResult<'a, 'b, 'c> = 
            async.Delay(generator >> Async.ofAsyncResult) |> AR
        
        member __.Bind(asyncResult : AsyncResult<'a, 'msg, 'rollback>, binder : 'a -> AsyncResult<'b, 'msg, 'rollback>) : AsyncResult<'b, 'msg, 'rollback> = 
            let fSuccess (value, msgs, rollbacks) = 
                value |> (binder
                          >> Async.ofAsyncResult
                          >> Async.map (mergeMessages msgs >> mergeRollbacks rollbacks))
            
            let fFailure errs = 
                errs
                |> Bad
                |> Async.singleton
            
            asyncResult
            |> Async.ofAsyncResult
            |> Async.bind (either fSuccess fFailure)
            |> AR
        
        member this.Bind(result : Result<'a, 'msg, 'rollback>, binder : 'a -> AsyncResult<'b, 'msg, 'rollback>) : AsyncResult<'b, 'msg, 'rollback> = 
            this.Bind(result
                      |> Async.singleton
                      |> AR, binder)
        
        member __.Bind(async : Async<'a>, binder : 'a -> AsyncResult<'b, 'msg, 'rollback>) : AsyncResult<'b, 'msg, 'rollback> = 
            async
            |> Async.bind (binder >> Async.ofAsyncResult)
            |> AR
        
        member __.TryWith(asyncResult : AsyncResult<'a, 'msg, 'rollback>, catchHandler : exn -> AsyncResult<'a, 'msg, 'rollback>) : AsyncResult<'a, 'msg, 'rollback> = 
            async.TryWith(asyncResult |> Async.ofAsyncResult, (catchHandler >> Async.ofAsyncResult)) |> AR
        member __.TryFinally(asyncResult : AsyncResult<'a, 'msg, 'rollback>, compensation : unit -> unit) : AsyncResult<'a, 'msg, 'rollback> = 
            async.TryFinally(asyncResult |> Async.ofAsyncResult, compensation) |> AR
        member __.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> AsyncResult<'a, 'msg, 'rollback>) : AsyncResult<'a, 'msg, 'rollback> = 
            async.Using(resource, (binder >> Async.ofAsyncResult)) |> AR
    
    // Wraps async computations in an error handling computation expression.
    let asyncTrial = AsyncTrialBuilder()

namespace Chessie.ErrorHandling.CSharp

open System
open System.Runtime.CompilerServices
open Chessie.ErrorHandling

/// Extensions methods for easier C# usage.
[<Extension>]
type ResultExtensions () =
    /// Allows pattern matching on Results from C#.
    [<Extension>]
    static member inline Match(this, ifSuccess:Action<'TSuccess , ('TMessage list)>, ifFailure:Action<'TMessage list>) =
        match this with
        | Result.Ok(x, msgs, _) -> ifSuccess.Invoke(x,msgs)
        | Result.Bad(msgs, _) -> ifFailure.Invoke(msgs)
    
    /// Allows pattern matching on Results from C#.
    [<Extension>]
    static member inline Either(this, ifSuccess:Func<'TSuccess , ('TMessage list),'TResult>, ifFailure:Func<'TMessage list,'TResult>) =
        match this with
        | Result.Ok(x, msgs,_) -> ifSuccess.Invoke(x,msgs)
        | Result.Bad(msgs,_) -> ifFailure.Invoke(msgs)

    /// Lifts a Func into a Result and applies it on the given result.
    [<Extension>]
    static member inline Map(this:Result<'TSuccess, 'TMessage, 'TRollback>,func:Func<_,_>) =
        lift func.Invoke this

    /// Collects a sequence of Results and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    [<Extension>]
    static member inline Collect(values:seq<Result<'TSuccess, 'TMessage, 'TRollback>>) =
        collect values

    /// Collects a sequence of Results and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    [<Extension>]
    static member inline Flatten(this) : Result<seq<'TSuccess>,'TMessage,'TRollback>=
        match this with
        | Result.Ok(values:seq<Result<'TSuccess,'TMessage,'TRollback>>, _:List<'TMessage>, _:List<'TRollback>) -> 
            match collect values with
            | Result.Ok(values,msgs,rs) -> Ok(values |> List.toSeq,msgs,rs)
            | Result.Bad(msgs:'TMessage list, rs:'TRollback list) -> Bad (msgs,rs)
        | Result.Bad(msgs:'TMessage list, rs) -> Bad (msgs,rs)

    /// If the result is a Success it executes the given Func on the value.
    /// Otherwise the exisiting failure is propagated.
    [<Extension>]
    static member inline SelectMany (this:Result<'TSuccess, 'TMessage, 'TRollback>, func: Func<_,_>) =
        bind func.Invoke this

    /// If the result is a Success it executes the given Func on the value.
    /// If the result of the Func is a Success it maps it using the given Func.
    /// Otherwise the exisiting failure is propagated.
    [<Extension>]
    static member inline SelectMany (this:Result<'TSuccess, 'TMessage, 'TRollback>, func: Func<_,_>, mapper: Func<_,_,_>) =
        bind (fun s -> s |> func.Invoke |> lift (fun v -> mapper.Invoke(s,v))) this

    /// Lifts a Func into a Result and applies it on the given result.
    [<Extension>]
    static member inline Select (this:Result<'TSuccess, 'TMessage, 'TRollback>, func: Func<_,_>) = lift func.Invoke this

    /// Returns the error messages or fails if the result was a success.
    [<Extension>]
    static member inline FailedWith(this:Result<'TSuccess, 'TMessage, 'TRollback>) = 
        match this with
        | Result.Ok(v,msgs,_) -> failwithf "Result was a success: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
        | Result.Bad(msgs,_) -> msgs

    /// Returns the result or fails if the result was an error.
    [<Extension>]
    static member inline SucceededWith(this:Result<'TSuccess, 'TMessage, 'TRollback>) : 'TSuccess = 
        match this with
        | Result.Ok(v,_msgs,_) -> v
        | Result.Bad(msgs,_) -> failwithf "Result was an error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))

    /// Joins two results. 
    /// If both are a success the resultSelector Func is applied to the values and the existing success messages are propagated.
    /// Otherwise the exisiting error messages are propagated.
    [<Extension>]
    static member inline Join (this: Result<'TOuter, 'TMessage, 'TRollback>, inner: Result<'TInner, 'TMessage, 'TRollback>, _outerKeySelector: Func<'TOuter,'TKey>, _innerKeySelector: Func<'TInner, 'TKey>, resultSelector: Func<'TOuter, 'TInner, 'TResult>) =
        let curry func = fun a b -> func (a, b)
        curry resultSelector.Invoke
        <!> this 
        <*> inner

    /// Converts an option into a Result.
    [<Extension>]
    static member ToResult(this, msg) =
        this |> failIfNone msg

    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded.
    [<Extension>]
    static member inline MapFailure (this: Result<'TSuccess, 'TMessage, 'TRollback>, f: Func<'TMessage list, 'TMessage2 seq>) =
        this |> Trial.mapFailure (f.Invoke >> Seq.toList)

