namespace hej

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO
open ScrabbleUtil.DebugPrint
open State

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand (pieces: Map<uint32,tile>) hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()
    
    let printHandSize (hand: MultiSet<uint32>) = forcePrint ((size hand).ToString())
    
    let printTiles (pieces: Map<uint32,tile>) =
        Map.fold(fun acc id tile ->  
                            let myString = $"Tile: %d{id} %c{(fst (List.head (Set.toList tile)))} %d{(snd (List.head (Set.toList tile)))   }"
                            acc + myString)
                            "" pieces


module Algorithm =

    let makeCoords (playHorizontal: bool) (startCoord: (int * int)) (lengthOfWordToPlay) (isFirstMove: bool)= 
        let (myCoordList: (int*int) list) = []
        debugPrint (sprintf "\nMake coords called with start coords: x: %d y: %d \n" (fst startCoord) (snd startCoord))

        let rec aux (acc: (int*int) list) i = 
            
            if (i < lengthOfWordToPlay) then
                if isFirstMove then
                    //the first move is always horizontal
                    let newAcc = (i, snd startCoord) :: acc
                    aux newAcc (i+1)
                else 
                    if playHorizontal then
                        debugPrint "\nWe want to play horizontally\n"
                        let newCoord = ((fst startCoord)+1 + i, (snd startCoord))
                        //debugPrint (sprintf "made a new coord: x: %d y: %d \n" (fst newCoord) (snd newCoord))
                        let newAcc = (newCoord) :: acc
                        aux newAcc (i+1)
                    else
                        debugPrint "\nWe want to play vertically\n"
                        let newCoord = ((fst startCoord), (snd startCoord)+1 + i)
                        //debugPrint (sprintf "made a new coord: x: %d y: %d \n" (fst newCoord) (snd newCoord))
                        let newAcc = (newCoord) :: acc
                        aux newAcc (i+1)
                        
            else acc
        aux myCoordList 0

        
       |> (fun lstToFlip -> List.rev(lstToFlip))         
        

// ------------ ALGORITHM ------------
type Direction =
| Horizontal
| Vertical


module Scrabble =
    open System.Threading
    open Algorithm
    open Moves

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            debugPrint "\n AUX WAS CALLED NEW ROUND \n" 
            Print.printHand pieces (State.hand st)

            let result = getNextMoveToPlay st pieces
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "\n Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
        
            let input = ""
            let move = RegEx.parseMove input

    
            send cstream (SMPlay move)
        
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand =
                    List.fold(fun hand x -> removeSingle (fst(snd x)) hand) st.hand ms |>
                    (fun removeTiles -> removeTiles, newPieces) ||>  // remove old tiles
                    List.fold(fun hand (id, numberOf) -> add id numberOf hand)  // add new ones
                
                let updateOccSquares =
                    List.fold(fun squares (coord,(id,(_,_))) -> Map.add coord id squares) st.occupiedSquares ms // update used squares

                let updatedNextTileId = fst (snd (ms.[ms.Length-1]))
                let updatedNextTileCoord = fst ms.[ms.Length-1]
                let nextTileUpdated = ((updatedNextTileId, updatedNextTileCoord), not (snd st.nextTile))
                let st' = {st with hand = newHand; occupiedSquares = updateOccSquares; nextTile = nextTileUpdated} // This state needs to be update    aux st'
                aux st'

            | RCM (CMChangeSuccess (newPieces)) ->
                debugPrint "WE swapped tiles and now our hand is: \n"
                let newChangedHand = List.fold(fun hand (id, numOf) -> add id numOf hand) empty newPieces
                let st' = {st with hand = newChangedHand} // This state needs to be update    aux st'
                Print.printHand pieces (State.hand st)
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st
  

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
       
        
        
        //testing with always getting letters for the word NO
        //testtiles
        
        let (testTile1: tile) = Set.ofList([('O', 1)])
        let (testTile2: tile) = Set.ofList([('N', 1)])
        let (testTile3: tile) = Set.ofList(['F', 2])
        let (testTile4: tile) = Set.ofList(['T', 2])

        let (testTilesMap: Map<uint32, tile>) = Map[(15u, testTile1); (14u, testTile2); (6u, testTile3); (20u, testTile4)]
        
        //testhand
        let (testHandList: (uint32 * uint32) list) = [(14u, 1u); (20u, 1u); (15u, 1u); (6u,1u)]
        let testHandSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty testHandList
        
        //the original one
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
      
        //fun () -> playGame cstream testTilesMap (State.mkState board dict playerNumber testHandSet)
        
        //set in to play for now
        //(0 0 14N1) (0 1 15O1)
        
 
  