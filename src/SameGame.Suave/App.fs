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
    | NewGame of GameConfig * AsyncReplyChannel<Guid option>
    | Play of Guid * int * int
    | StartOver of Guid
    | Game of Guid * AsyncReplyChannel<Game option>

let parse str f = 
    match str |> f with 
    | (true,i) -> Some i 
    | _        -> None

let toInt str = parse str Int32.TryParse

let toGuid str = parse str Guid.TryParse

module Interactions =
    open SameGame.Types
    open FSharpx.Option

    let private transform (board: int list list) = 
        [for row in [(board.[0].Length - 1)..(-1)..0] do yield ([0..(board.Length - 1)] |> List.map (fun col -> board.[col].[row]))]

    let private mapBoardtoVm (id, gameState) = 
        let map b = b |> List.map (fun col -> col |> List.map (function Stone (Color c) -> c | _ -> 0)) |> transform
        let makeVm gs msg = { Id = id; BoardVm.Score = gs.Score; Board = map gs.Board; Status = msg }
        match gameState with
        | InProgress gs -> makeVm gs "In Progress"
        | Finished gs   -> makeVm gs "Finished"

    let getGame (agent:Agent<SameGameMessage>) id = 
        maybe {
            let! guid = id |> toGuid
            let! game = agent.PostAndReply(fun replyChannel -> Game (guid, replyChannel))
            return guid, game }
        |> function
        | Some gs -> page "play.html" (gs |> mapBoardtoVm)
        | _       -> RequestErrors.NOT_FOUND "game not found"

    let play (agent:Agent<SameGameMessage>) (id:Choice<string, string>) (row:Choice<string, string>) (col:Choice<string, string>) = 
        maybe {
            let! guid = id |> Option.ofChoice |> Option.bind toGuid
            let! c = col |> Option.ofChoice |> Option.bind toInt
            let! r = row |> Option.ofChoice |> Option.bind toInt
            do agent.Post(Play (guid, c,r))
            return guid }
        |> function
        | Some i -> Redirection.redirect (sprintf "/game/%O" i)
        | _      -> RequestErrors.BAD_REQUEST "invalid request"

    let newGame (agent:Agent<SameGameMessage>) (colors:Choice<string, string>) (cols:Choice<string, string>) (rows:Choice<string, string>) = 
        maybe {
            let! n = colors |> Option.ofChoice |> Option.bind toInt
            let! c = cols |> Option.ofChoice |> Option.bind toInt
            let! r = rows |> Option.ofChoice |> Option.bind toInt
            let conf = { NumberOfColumns = c; NumberOfRows = r; MaxNumberOfColors = n } 
            let! id = agent.PostAndReply(fun replyChannel -> NewGame (conf, replyChannel)) 
            return id }
        |> function
        | Some i -> Redirection.redirect (sprintf "/game/%O" i)
        | _      -> RequestErrors.BAD_REQUEST "invalid request"

    let startOver (agent:Agent<SameGameMessage>) (id:Choice<string, string>) =
        maybe {
            let! guid = id |> Option.ofChoice |> Option.bind toGuid
            do agent.Post(StartOver(guid))
            return guid }
        |> function
            | Some i -> Redirection.redirect (sprintf "/game/%O" i)
            | _      -> RequestErrors.BAD_REQUEST "invalid request"

let sameGameAgent maxNumberOfGames api = 
    let filterGames =
        List.sortBy (fun (_,(dt,_,_)) -> dt)
        >> List.rev
        >> fun xs -> 
            if xs |> List.length <= maxNumberOfGames then xs 
            else xs |> List.take maxNumberOfGames

    let tryFindGame (gameList : (Guid * (DateTime * Game * Game)) list) id = gameList.TryFind (fun (gameId, _) -> gameId = id)

    Agent.Start(fun inbox ->
        let rec loop gameList = async {
            let! msg = inbox.Receive()
            match msg with 
            | NewGame (config, repl) ->
                let newGame = api.NewGame config
                let id = Guid.NewGuid()
                do repl.Reply(newGame |> Option.map (fun _ -> id))
                match newGame with
                | Some g -> return! gameList @ [(id, (DateTime.Now, g, g))] 
                                    |> filterGames 
                                    |> loop
                | _ -> return! loop gameList
            | Play (id, col, row) ->
                match id |> tryFindGame gameList with
                | Some (_, (_, init, current)) -> 
                    let newGameState = api.Play current { Col = col; Row = row }
                    return! gameList 
                            |> List.map (fun (key, value) -> 
                                            if key = id then (key, (DateTime.Now, init, newGameState)) 
                                            else (key, value)) 
                            |> filterGames
                            |> loop
                | _ -> return! loop gameList
            | StartOver id ->
                match id |> tryFindGame gameList with
                | Some (_, (_, initial, _)) -> 
                    return! gameList 
                            |> List.map (fun (key, (dt, init, current)) -> 
                                            if key = id then (key, (DateTime.Now, init, init))
                                            else (key, (dt, init, current))) 
                            |> filterGames
                            |> loop
                | _ -> return! loop gameList
            | Game (id, repl) -> 
                id |> tryFindGame gameList
                |> Option.map (fun (_, (_, _, state)) -> state)
                |> repl.Reply
                return! loop gameList }
        loop [])

open SameGame.Domain
open System.Net

[<EntryPoint>]
let main [| port; staticFilesLocation |] =
    let maxNumberOfGames = 500

    let webDir = IO.Path.Combine(Environment.CurrentDirectory, staticFilesLocation)
    let style = File.ReadAllText (IO.Path.Combine(webDir, "bootstrap.css"))
    let siteCss = File.ReadAllText (IO.Path.Combine(webDir, "site.css"))
    let jQuery = File.ReadAllText (IO.Path.Combine(webDir, "jquery-1.12.0.min.js"))

    DotLiquid.setTemplatesDir webDir

    let agent = sameGameAgent maxNumberOfGames api 

    let getGame = Interactions.getGame agent
    let play = Interactions.play agent
    let newGame = Interactions.newGame agent
    let startOver = Interactions.startOver agent

    let app =
        choose 
            [ GET >=> choose 
                [ path "/" >=> request (fun _ -> page "index.html" ())
                  pathScan "/game/%s" getGame
                  path "/rules" >=> request (fun _ -> page "rules.html" ())
                  path "/static/jquery-1.12.0.min.js" >=> Writers.setMimeType "text/javascript" >=> OK jQuery
                  path "/static/style.css" >=> Writers.setMimeType "text/css" >=> OK style
                  path "/static/site.css" >=> Writers.setMimeType "text/css" >=> OK siteCss ]
              POST >=> choose 
                [ path "/newgame" >=> request (fun r -> newGame (r.formData "colors") (r.formData "columns") (r.formData "rows"))
                  path "/startover" >=> request (fun r -> startOver(r.formData "gameid"))
                  path "/play" >=> request (fun r -> play (r.formData "gameid") (r.formData "row") (r.formData "col")) ] 
              RequestErrors.NOT_FOUND "Found no handlers"]

    let config = { defaultConfig with bindings = [ HttpBinding.mk HTTP IPAddress.Loopback (uint16 port) ] }

    startWebServer config app
    0