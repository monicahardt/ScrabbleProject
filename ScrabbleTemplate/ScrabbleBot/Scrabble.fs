namespace hej

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO
open ScrabbleUtil.DebugPrint
open State

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

type Direction =
| Horizontal
| Vertical


module Scrabble =
    open System.Threading
    open Moves
    open MoveUtilities

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            debugPrint "\n AUX WAS CALLED NEW ROUND \n" 
            Print.printHand pieces (State.hand st)

            //test pieces st
            let result = getNextMoveToPlay st pieces
            
            let resultArray = Map.toArray result
            if resultArray.Length > 0 then
                let longestFoundWord = Array.fold(fun (acc: uint32 list) ((word: uint32 list),(coords: coord list))-> if word.Length >= acc.Length then word else acc ) [] resultArray
                let coordsToWord = result.[longestFoundWord]

                if st.occupiedSquares.IsEmpty then
                    //debugPrint "\noccupied squares was empty\n"
                    let move = makeMove longestFoundWord pieces coordsToWord
                    //debugPrint "\nMADE THE MOVE\n"
                    send cstream (SMPlay move)
                else 
                    //debugPrint "\noccupied squares was not empty\n"
                    let word =  makeAWord (longestFoundWord) pieces
                    debugPrint (sprintf "\nTHE WORD WE ARE PLAYING IS: %s\n" word)
                    let move = makeMove longestFoundWord.[1..] pieces coordsToWord.[1..]
                    //debugPrint "\nMADE THE MOVE NOT FIRST\n"
                    send cstream (SMPlay move)
            else 
                debugPrint "\nWE COULD NOT PLACE A WORD SWAPPING TILES\n"
                send cstream (SMChange (getHandAsList st.hand))
                
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    removeTilesFromHand st ms |>
                    addNewTiles newPieces |>           
                    updateOccSquares ms
                
                aux st'

            | RCM (CMChangeSuccess (newPieces)) ->
                let st' = changeHand newPieces st
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
            | RGPE err ->
                List.fold(fun acc thisError -> 
                    match thisError with
                    |GPENotEnoughPieces(_,piecesLeft) ->
                        if (int (piecesLeft) > 0) then send cstream (SMChange((getHandAsList st.hand).[0..(int piecesLeft)])) 
                        else send cstream SMPass
                    |_ ->
                        printfn "Gameplay Error:\n%A" err; aux st
                ) () err
            //| RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

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
    
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
      
        
 
  