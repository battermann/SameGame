namespace SameGame

module Domain =

    open System
    open Types

    let private bonus = 1000

    let private getCellState (board:Board) pos =
        let colCount = board |> List.length
        if pos.Col < colCount && pos.Col >= 0 && pos.Row < board.[pos.Col].Length && pos.Row >= 0 then
            board.[pos.Col].[pos.Row]
        else Empty

    let private findAdjacentWithSameColor board col (pos:Position) =
        [pos.Up; pos.Right; pos.Down; pos.Left]
        |> List.map (fun p ->  getCellState board p, p)
        |> List.filter (fun cell -> fst cell = Stone col)
        |> List.map snd

    let private hasValidMoves board = 
        board
        |> Seq.mapi (fun i col -> 
            col 
            |> Seq.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}))
        |> Seq.exists (fun col -> 
            col 
            |> Seq.exists (fun cell -> 
                match cell.State with 
                | Stone c -> cell.Position |> findAdjacentWithSameColor board c  |> (not << List.isEmpty) 
                | _       -> false))

    let private evaluateGameState gameState =
        if gameState.Board |> hasValidMoves then
            InProgress gameState 
        else
            Finished gameState

    let private getGroup board position =
        let rec find (ps:Position list) col (group:Position list) =
            match ps with
            | []    -> group
            | x::xs -> 
                let cells = x |> findAdjacentWithSameColor board col
                            |> List.filter (fun pos -> not (List.exists ((=) pos) (xs @ group) ))
                find (cells @ xs) col (x :: group)

        getCellState board position
        |> function 
            | Stone c -> 
                let positions = find [position] c []
                if positions |> List.length > 1 then
                    Some { Color = c; Positions = positions }
                else None
            | _ -> None

    let private removeGroup group board =
        board
        |> List.mapi (fun i col -> 
            col 
            |> List.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}) 
            |> List.filter (fun cell -> group.Positions |> (not << List.exists ((=) cell.Position)))
            |> List.map (fun cell -> cell.State)
            |> fun col' -> col' @ List.replicate (col.Length - col'.Length) Empty)
        |> List.filter (List.head >> ((<>) Empty))
        |> fun cols -> cols @ List.replicate (board.Length - cols.Length) (List.replicate (board.[0].Length) Empty)

    let private square x = x * x

    let private isEmpty (board:Board) = board |> List.forall (List.head >> ((=) Empty))

    let private getScore board group =
        square (group.Positions.Length - 2)
        + if board |> isEmpty then bonus else 0

    let private play gameState pos = 
        getGroup gameState.Board pos
        |> function 
            | Some g -> 
                let newBoard = gameState.Board |> removeGroup g
                { Board = newBoard; Score = gameState.Score + getScore newBoard g }
            | _ -> gameState

    let private playIfRunning game pos =
        match game with
        | InProgress gameState -> play gameState pos |> evaluateGameState
        | _                    -> game

    let private checkConfig conf onValid onInvalid =
        if conf.MaxNumberOfColors < 3 || conf.MaxNumberOfColors > 8 then
            onInvalid
        elif conf.NumberOfColumns < 3 || conf.NumberOfColumns > 20 then
            onInvalid
        elif conf.NumberOfRows < 3 || conf.NumberOfRows > 12 then
            onInvalid
        else
            onValid conf

    let private newGame conf = 
        let rnd = new Random()
        let makeBoard conf = 
            let board = List.init conf.NumberOfColumns (fun _ -> List.init conf.NumberOfRows (fun _ -> rnd.Next(1, int conf.MaxNumberOfColors + 1) |> Color |> Stone))
            { Board = board; Score = 0 } |> evaluateGameState |> Some
        checkConfig conf makeBoard None

    let api = {
        NewGame = newGame
        Play = playIfRunning }