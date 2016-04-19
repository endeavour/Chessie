(*** hide ***)
#I "../../bin"

(**

Using Chessie for Railway-oriented programming (ROP)
====================================================

This tutorial is based on an article about [Railway-oriented programming](http://fsharpforfunandprofit.com/posts/recipe-part2/) by Scott Wlaschin.

Additional resources:

* Railway Oriented Programming by Scott Wlaschin - A functional approach to error handling 
    * [Slide deck](http://www.slideshare.net/ScottWlaschin/railway-oriented-programming)
    * [Video](https://vimeo.com/97344498)

We start by referencing Chessie and opening the ErrorHandling module:
*)

#r "Chessie.dll"

open Chessie.ErrorHandling

let rng = System.Random()

let emptyFuel() =
  printfn "draining fuel"

let refuel() =
  printfn "refueling"
  if rng.NextDouble() < 0.7
    then okWithRollback emptyFuel ()
    else failWithRollback emptyFuel "Refueling failed"

let resetCodes() =
  printfn "reset launch codes"

let enterCodes() =
  printfn "entering launch codes"
  if rng.NextDouble() < 0.7
    then okWithRollback resetCodes ()
    else failWithRollback resetCodes "Launch codes sequence failed"

let cancelLaunchSequence() =
  printfn "cancel launch sequence"

let startLaunchSequence() =
  printfn "begin launch sequence"
  if rng.NextDouble() < 0.7
    then okWithRollback cancelLaunchSequence ()
    else failWithRollback cancelLaunchSequence "Launch codes sequence failed"

let fireMissile =
  trial {
    printfn "STARTING MISSILE FIRE SEQUENCE"
    do! refuel()
    do! enterCodes()
    do! startLaunchSequence()
  }

let printWarnings ms =
  ms
  |> List.iter (printfn "%s")

match fireMissile with
| Result.Ok(_,ms,_) ->
  printfn "MISSILE LAUNCH COUNTDOWN INITIATED"
| Result.Bad(ms,_) ->
  printfn "MISSILE LAUNCH COUNTDOWN COULD NOT BE STARTED"


printfn "ROLLING BACK"
(rollback fireMissile)()


// [fsi:DEBUG. Success so far.]
// [fsi:val it : Request = {Name = "Scott";]
// [fsi:                    EMail = "scott@chessie.com";}]
