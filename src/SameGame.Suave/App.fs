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
    Id:Guid
    Score:int
    Board:int list list
    Status:string }

#nowarn "40"
type Agent<'T> = MailboxProcessor<'T>

type SameGameMessage = 
    | NewGame of GameConfig * AsyncReplyChannel<(Guid*Game) option>
    | Play of Guid * int * int * AsyncReplyChannel<(Guid*Game) option>
    | StartOver of Guid * AsyncReplyChannel<(Guid*Game) option>

module Interactions =
    open SameGame.Types
    open FSharpx.Option

    let transform (board: int list list) = 
        [for row in [(board.[0].Length - 1)..(-1)..0] do yield ([0..(board.Length - 1)] |> List.map (fun col -> board.[col].[row]))]

    let mapBoardtoVm (id, gameState) = 
        let map b = b |> List.map (fun col -> col |> List.map (function Stone (Color c) -> c | _ -> 0)) |> transform
        let makeVm gs msg = { Id = id; BoardVm.Score = gs.Score; Board = map gs.Board; Status = msg }
        match gameState with
        | InProgress gs -> makeVm gs "In Progress"
        | Finished gs   -> makeVm gs "Finished"

    let parse str f = 
        match str |> f with 
        | (true,i) -> Some i 
        | _        -> None

    let toInt str = parse str Int32.TryParse

    let toGuid str = parse str Guid.TryParse

    let play (agent:Agent<SameGameMessage>) (id:Choice<string, string>) (row:Choice<string, string>) (col:Choice<string, string>) = 
        let gameOpt = maybe {
            let! guid = id |> Option.ofChoice |> Option.bind toGuid
            let! c = col |> Option.ofChoice |> Option.bind toInt
            let! r = row |> Option.ofChoice |> Option.bind toInt
            let! game = agent.PostAndReply(fun replyChannel -> Play (guid, c,r,replyChannel))
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

    let startOver (agent:Agent<SameGameMessage>) (id:Choice<string, string>) =
        maybe {
            let! guid = id |> Option.ofChoice |> Option.bind toGuid
            let! game = agent.PostAndReply(fun replyChannel -> StartOver(guid, replyChannel))
            return game }
        |> function
            | Some g -> page "play.html" (g |> mapBoardtoVm)
            | _      -> RequestErrors.BAD_REQUEST "invalid request"

let sameGameAgent api = 

    let maxNumberOfGames = 200
    let days = 1.0

    let filterGames =
        List.filter (fun (_, ((dt:DateTime),_,_)) -> dt.AddDays(days) > DateTime.Now)
        >> List.sortBy (fun (_, (dt,_,_)) -> dt)
        >> List.rev
        >> fun xs -> if xs |> List.length <= maxNumberOfGames then xs else xs |> List.take maxNumberOfGames

    Agent.Start(fun inbox ->
    
        let rec loop map = async {
            let! msg = inbox.Receive()
            match msg with 
            | NewGame (conf, repl) ->
                let newGame = api.NewGame conf
                let id = Guid.NewGuid()
                repl.Reply(newGame |> Option.map (fun g -> id, g))
                return! loop (map @ [(id, (DateTime.Now, newGame, newGame))] |> filterGames)
            | Play (id, col, row, repl) ->
                let state = Option.maybe {
                    let! (_, (_, initial, game)) = map |> List.tryFind (fun e -> fst e = id)
                    let newGameState = game |> Option.map (fun g -> api.Play g { Col = col; Row = row })
                    return initial, newGameState }
                match state with
                | Some s -> do repl.Reply(s |> snd |> Option.map (fun g -> id, g))
                            return! loop (map |> List.map (fun (key, value) -> if key = id then (key, (DateTime.Now, fst s, snd s)) else (key, value)) |> filterGames)
                | _      -> do repl.Reply(None)
                            return! loop (map |> List.filter (fun (key, _) -> key <> id) |> filterGames)
            | StartOver (id, repl) ->
                match map.TryFind (fun e -> fst e = id) with
                | Some (_, (_, initial, _)) -> do repl.Reply(initial |> Option.map (fun g -> id, g))
                                               return! loop (map |> List.map (fun (key, (dt,initial, game)) -> if key = id then (key,( DateTime.Now, initial, initial)) else (key, (dt, initial, game))) |> filterGames)
                | _                         -> do repl.Reply(None)
                                               return! loop (map |> List.filter (fun (key, _) -> key <> id) |> filterGames) }
        loop [])

open SameGame.Domain
open System.Net

[<EntryPoint>]
let main [| port; staticFilesLocation |] =

    let agent = sameGameAgent api

    let play = Interactions.play agent
    let newGame = Interactions.newGame agent
    let startOver = Interactions.startOver agent

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
                  path "/startover" >=> request (fun r -> startOver(r.formData "gameid"))
                  path "/play" >=> request (fun r -> play (r.formData "gameid") (r.formData "row") (r.formData "col")) ] 
              RequestErrors.NOT_FOUND "Found no handlers"]

    startWebServer config app
    0