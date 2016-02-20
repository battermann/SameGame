namespace SameGame.Domain.Tests

open FsUnit
open NUnit.Framework
open System
open FSharpx.Collections
open FSharpx
open FsCheck.NUnit

module TestHelper = 
    open SameGame.Types
    let createBoard =
        List.map (fun col -> col |> List.map (function 0 -> Empty | c -> c |> Color |> Stone))

module ``Given the board is empty`` =
    open SameGame.Types
    open SameGame.Domain
    open TestHelper

    let gs = Finished { Board = [[0;0];[0;0];[0;0]] |> createBoard; Score = 0 }

    [<Property>]
    let ``when making a play the GameState should not change`` (pos:Position) =
        api.Play gs pos = gs
        
module SameGameTests =
    open SameGame.Types
    open SameGame.Domain
    open TestHelper

    // board:
    // 3 3 2
    // 2 1 2
    // 1 1 2 

    [<Test>]
    let ``play 0,2 should remove group`` () =
        let gs = InProgress { Board = [[1;2;3]; [1;1;3];[2;2;2]] |> createBoard; Score = 0 }
        let pos = { Col = 0; Row = 2 }
        let expected = InProgress { Board = [[1;2;0]; [1;1;0];[2;2;2]] |> createBoard; Score = 0}
        let actual = api.Play gs pos
        actual |> should equal expected

    [<Test>]
    let ``play 0,0 should remove group and score + 1`` () =
        let gs = InProgress { Board = [[1;2;0]; [1;1;0];[2;2;2]] |> createBoard; Score = 0}
        let pos = { Col = 0; Row = 0 }
        let expected = InProgress { Board = [[2;0;0]; [2;2;2];[0;0;0]] |> createBoard; Score = 1}
        let actual = api.Play gs pos
        actual |> should equal expected

    [<Test>]
    let ``play 0,0 should remove group and score + 1004`` () =
        let gs = InProgress { Board = [[2;0;0]; [2;2;2];[0;0;0]] |> createBoard; Score = 1}
        let pos = { Col = 0; Row = 0 }
        let expected = Finished { Board = [[0;0;0]; [0;0;0];[0;0;0]] |> createBoard; Score = 1005 }
        let actual = api.Play gs pos
        actual |> should equal expected

    [<Test>]
    let ``play empty cell should do nothing`` () =
        let gs = InProgress { Board = [[2;0;0]; [2;2;2];[0;0;0]] |> createBoard; Score = 1}
        let pos = { Col = 0; Row = 3 }
        let expected = InProgress { Board = [[2;0;0]; [2;2;2];[0;0;0]] |> createBoard; Score = 1}
        let actual = api.Play gs pos
        actual |> should equal expected

    [<Test>]
    let ``play singleton group should do nothing`` () =
        let gs = InProgress { Board = [[1;2;3]; [1;1;3];[2;2;2]] |> createBoard; Score = 0}
        let pos = { Col = 0; Row = 1 }
        let expected = InProgress { Board = [[1;2;3]; [1;1;3];[2;2;2]] |> createBoard; Score = 0}
        let actual = api.Play gs pos
        actual |> should equal expected