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
        fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()
    
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
    open Moves
    open MoveUtilities

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) (isMyTurn : bool) =
            if (isMyTurn) then
                debugPrint "\n AUX WAS CALLED NEW ROUND \n" 
                Print.printHand pieces (State.hand st)

                let result = getNextMoveToPlay st pieces
                let resultArray = Map.toArray result

                if resultArray.Length > 0 then 
                    let longestFoundWord = 
                        Array.fold(fun (acc: uint32 list) ((word: uint32 list),_) -> 
                        if word.Length >= acc.Length then word else acc ) [] resultArray
                    let coordsToWord = result.[longestFoundWord]

                    if st.occupiedSquares.IsEmpty then //the first word we play, so we want all the tiles 
                        let move = makeMove longestFoundWord pieces coordsToWord
                        send cstream (SMPlay move)
                    else //we play out from a tile that is already placed on the board, so we ignore [0]
                        let move = makeMove longestFoundWord.[1..] pieces coordsToWord.[1..]
                        send cstream (SMPlay move)
                else //we couldn't make any valid moves with our hands -> swap tiles
                    send cstream (SMChange (getHandAsList st.hand))

            let msg = recv cstream
            
            match msg with
            
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    removeTilesFromHand st ms |>
                    addNewTiles newPieces |>           
                    updateOccSquares ms
                aux st' false
            | RCM (CMChangeSuccess (newPieces)) ->
                let st' = changeHand newPieces st
                aux st' false
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st |> updateOccSquares ms // This state needs to be updated
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st |> updateOccSquares ms // This state needs to be updated
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMPassed pid) -> aux st (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                List.fold(fun _ thisError -> 
                    match thisError with
                    |GPENotEnoughPieces(_,piecesLeft) ->
                        if (int (piecesLeft) > 0) then 
                            send cstream (SMChange((getHandAsList st.hand).[0..(int piecesLeft)])) ; aux st false
                        else send cstream (SMPass); aux st false
                    |_ ->
                        printfn "Gameplay Error:\n%A" err; aux st false
                ) () err

        if (st.playerTurn = st.playerNumber) then
            aux st true
        else 
            aux st false
  
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
                  
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand
    
        fun () -> playGame cstream tiles (mkState board dict numPlayers playerNumber playerTurn handSet)
      
        
 
  