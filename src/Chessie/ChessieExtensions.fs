/// Contains additional error propagation functions and a computation expression builder for Railway-oriented programming.
namespace Chessie.ErrorHandling.Extensions

open System
open Chessie.ErrorHandling

module Trial =
  let failIfFalse e flag =
    if flag then ok () else fail e

module AsyncTrial =
  open Chessie.ErrorHandling
  
  let catch (x:Async<'a>) : AsyncResult<'a, exn, _> =
    async {
      let! wrapped = x |> Async.Catch 
      return Trial.ofChoice wrapped
    } |> AR

  let lift f = Async.ofAsyncResult >> Async.map f >> AR

  let mapSuccess f = lift (Trial.lift f)

  let mapFailure f = lift (Trial.mapFailure f)
