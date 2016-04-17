open System
open System.IO
open Suave
open Suave.Successful
open Suave.Web
open Suave.Http
open Suave.DotLiquid
open Suave.Operators
open Suave.Filters
open FSharpx
open SameGame.Types

type BoardVm = {
    Score:int
    Board:int list list
    Status:string }

#nowarn "40"
type Agent<'T> = MailboxProcessor<'T>

type SameGameMessage = 
    | NewGame of GameConfig * AsyncReplyChannel<Game option>
    | Play of int * int * AsyncReplyChannel<Game option>
    | StartOver of AsyncReplyChannel<Game option>

module Interactions =
    open SameGame.Types
    open FSharpx.Option

    let transform (board: int list list) = 
        [for row in [(board.[0].Length - 1)..(-1)..0] do yield ([0..(board.Length - 1)] |> List.map (fun col -> board.[col].[row]))]

    let mapBoardtoVm gameState = 
        let map b = b |> List.map (fun col -> col |> List.map (function Stone (Color c) -> c | _ -> 0)) |> transform
        let makeVm gs msg = { BoardVm.Score = gs.Score; Board = map gs.Board; Status = msg }
        match gameState with
        | InProgress gs -> makeVm gs "In Progress"
        | Finished gs   -> makeVm gs "Finished"

    let parse str f = 
        match str |> f with 
        | (true,i) -> Some i 
        | _        -> None

    let toInt str = parse str Int32.TryParse

    let play (agent:Agent<SameGameMessage>) (row:Choice<string, string>) (col:Choice<string, string>) = 
        let gameOpt = maybe {
            let! c = col |> Option.ofChoice |> Option.bind toInt
            let! r = row |> Option.ofChoice |> Option.bind toInt
            let! game = agent.PostAndReply(fun replyChannel -> Play (c,r,replyChannel))
            return game }

        match gameOpt with
        | Some g -> page "play.html" (g |> mapBoardtoVm)
        | _      -> RequestErrors.BAD_REQUEST "invalid request"

    let newGame (agent:Agent<SameGameMessage>) (colors:Choice<string, string>) (cols:Choice<string, string>) (rows:Choice<string, string>) = 
        let gameOpt = maybe {
            let! n = colors |> Option.ofChoice |> Option.bind toInt
            let! c = cols |> Option.ofChoice |> Option.bind toInt
            let! r = rows |> Option.ofChoice |> Option.bind toInt
            let conf = { NumberOfColumns = c; NumberOfRows = r; MaxNumberOfColors = n } 
            let! game = agent.PostAndReply(fun replyChannel -> NewGame (conf, replyChannel)) 
            return game }

        match gameOpt with
        | Some g -> page "play.html"  (g |> mapBoardtoVm)
        | _      -> RequestErrors.BAD_REQUEST "invalid request"

    let startOver (agent:Agent<SameGameMessage>) =
        agent.PostAndReply(fun replyChannel -> StartOver(replyChannel))
        |> function
            | Some g -> page "play.html" (g |> mapBoardtoVm)
            | _      -> RequestErrors.BAD_REQUEST "invalid request"

let sameGameAgent api = 
    Agent.Start(fun inbox ->
    
        let rec loop initial game = async {
            let! msg = inbox.Receive()
            match msg with 
            | NewGame (conf, repl) ->
                let newGame = api.NewGame conf
                repl.Reply(newGame)
                return! loop newGame newGame
            | Play (col, row, repl) ->
                let newGameState = game |> Option.map (fun g -> api.Play g { Col = col; Row = row })
                repl.Reply(newGameState)
                return! loop initial newGameState
            | StartOver repl -> 
                repl.Reply(initial)
                return! loop initial initial }

        loop None None)

open SameGame.Domain
open System.Net

[<EntryPoint>]
let main [| port; staticFilesLocation |] =

    let agent = sameGameAgent api

    let play = Interactions.play agent
    let newGame = Interactions.newGame agent
    let startOver() = Interactions.startOver agent

    let webDir = IO.Path.Combine(Environment.CurrentDirectory, staticFilesLocation)
    let style = File.ReadAllText (IO.Path.Combine(webDir, "bootstrap.css"))
    let siteCss = File.ReadAllText (IO.Path.Combine(webDir, "site.css"))
    let jQuery = File.ReadAllText (IO.Path.Combine(webDir, "jquery-1.12.0.min.js"))

    let config = { defaultConfig with bindings = [ HttpBinding.mk HTTP IPAddress.Loopback (uint16 port) ] }

    DotLiquid.setTemplatesDir webDir

    let app = 
        choose 
            [ GET >=> choose 
                [ path "/" >=> request (fun _ -> page "index.html" ())
                  path "/rules" >=> request (fun _ -> page "rules.html" ())
                  path "/static/jquery-1.12.0.min.js" >=> Writers.setMimeType "text/javascript" >=> OK jQuery
                  path "/static/style.css" >=> Writers.setMimeType "text/css" >=> OK style
                  path "/static/site.css" >=> Writers.setMimeType "text/css" >=> OK siteCss ]
              POST >=> choose 
                [ path "/newgame" >=> request (fun r -> newGame (r.formData "colors") (r.formData "columns") (r.formData "rows"))
                  path "/startover" >=> request (fun _ -> startOver())
                  path "/play" >=> request (fun r -> play (r.formData "row") (r.formData "col")) ] 
              RequestErrors.NOT_FOUND "Found no handlers"]

    startWebServer config app
    0