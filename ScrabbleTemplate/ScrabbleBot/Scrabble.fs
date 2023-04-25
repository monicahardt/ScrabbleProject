﻿namespace hej

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

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
   
module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        occupiedSquares : Map<coord, uint32>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; occupiedSquares = Map.empty }
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Algorithm =
    open Dictionary
    
    // ------------ HELPER FUNCTIONS ------------
    let getCharFromId (pieces: Map<uint32,tile>) (id: uint32) =
        let tile = Map.find id pieces
        (fst (List.head (Set.toList tile)))
   
    let getListOfCharsFromHand (pieces: Map<uint32,tile>)  (st: State.state) =
        st.hand |> MultiSet.fold(fun lst id _ -> lst @ [getCharFromId pieces id]) []
   
    let getListIdsFromHand (st: State.state) =
        toList st.hand

    // ------------ ALGORITHM ------------
    let firstMove (st: State.state) (pieces: Map<uint32,tile>): Map<uint32, uint32 list> = 
        let handIds = getListIdsFromHand st

        //outer recursion should recurse over the hand
        let rec aux (i: int) (auxMap: Map<uint32,uint32 list>) = 
            if(i >= handIds.Length) 
            then 
                //there we not any tiles left on the hand to go through
                auxMap 
            else 
                let charWeAreAt = handIds.[i]
                debugPrint (sprintf ("aux running again the char is now %c \n" )(getCharFromId pieces handIds.[i])) 
                debugPrint (sprintf "the char we are checking is %d \n" charWeAreAt) 
                let handWithoutCharWeAreAt = removeSingle handIds.[i] st.hand
                let firstCharStepped = step (getCharFromId pieces handIds.[i]) st.dict
                
                match firstCharStepped with
                    |Some(b,d) -> //this should always happen
                        //adding the first id to the list result
                        debugPrint "Found the first char \n"
                        let map = Map.add charWeAreAt [charWeAreAt] auxMap
                            
                        //recursive function start
                        let rec lookForWord (dictionary: Dict) (MS: MultiSet<uint32>) (listOfPossibleWords: (uint32 list*bool)) =
                            debugPrint "******NEW RECURSION***** \n"
                        
                            //folding over the rest of the multiset
                            MultiSet.fold(fun (acc: (uint32 list * bool)) id _ ->
                                if (snd acc) then acc else

                                debugPrint (sprintf "folding over the char with id %d \n" id)
                                match (step (getCharFromId pieces id) dictionary) with
                                |Some(b,dict) ->
                                    //let updatedAcc = (List.rev (id :: (fst (acc)))) //there was a path to this id so we add it to the foldList
                                    let updatedHand = removeSingle id MS
                                    
                                    debugPrint (sprintf "there was a path to the char with id %d \n" id)
                                    debugPrint (sprintf "printing size of the hand we are sending on: %d \n" (size updatedHand))
                                   
                                    if b then debugPrint "a word has ended \n"  else debugPrint "continuing"
                            
                                    let updatedAccList = ((id :: (fst (acc)))) 
                                    lookForWord dict updatedHand (updatedAccList,b)

                                |None ->
                                    debugPrint (sprintf "there was not a path to the char with id %d \n" id)
                                    acc
                            ) listOfPossibleWords MS |> (fun x-> 
                                                                debugPrint "********A fold is complete. Aka a recursion is done******** \n" 
                                                                x) 
                        
                        //first call to lookForWord
                        let resultFromLookForWord = lookForWord d handWithoutCharWeAreAt (map.[charWeAreAt], false)
                        //adding to the map the word (list) found in the lookForWord function
                        let map = auxMap |> Map.add charWeAreAt (List.rev (fst resultFromLookForWord))
                        //aux called with i incremented
                        aux (i+1) map 
                            
                    |None -> 
                        failwith "This should not happen"
        
        //first call to aux with i being 0 and our map being empty
        aux 0 Map.empty                                    
           

// ------------ BOARD HANDLING ------------
    let createInputMove = failwith "not implemented"

    let findBoardPosition = failwith "not implemented"
   
module Scrabble =
    open System.Threading
    open Algorithm

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let resultMap = firstMove st pieces 

            Map.fold(fun _ id lst -> forcePrint (sprintf "printing a list %d \n" id) ) () resultMap

            Map.fold(fun acc id lst -> 
                debugPrint (sprintf "Folding over list belonging to id %d \n" id)
                List.fold(fun acc id -> forcePrint (string (getCharFromId pieces id))) () lst
                debugPrint ("\n ***** \n")
            ) () resultMap
            
            debugPrint ("printing the size of the map " + resultMap.Count.ToString())
           
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "\n Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
         
            
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input
            
          
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand =
                    List.fold(fun hand x -> removeSingle (fst(snd x)) hand) st.hand ms |>
                    (fun removeTiles -> removeTiles, newPieces) ||>  // remove old tiles
                    List.fold(fun hand (id, numberOf) -> add id numberOf hand)  // add new ones
                
                let updateOccSquares =
                    List.fold(fun squares (coord,(id,(_,_))) -> Map.add coord id squares) st.occupiedSquares ms // update used squares
                
                let st' = {st with hand = newHand; occupiedSquares = updateOccSquares} // This state needs to be update    aux st'
                
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
        //fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
      
        fun () -> playGame cstream testTilesMap (State.mkState board dict playerNumber testHandSet)
        
        //set in to play for now
        //(0 0 14N1) (0 1 15O1)
        
 
  (*
    let checkMove (myMoveList: (uint32*bool) list) (pieces: Map<uint32,tile>) (st: State.state) =
        forcePrint "check move was called \n"
        let rec aux (lst: (uint32*bool) list) =
            forcePrint "aux called"
            match snd(lst.[0]) with
            |true ->
                forcePrint "it was possible to make a word and therefor we make an input with this \n"
                makeInput [] myMoveList pieces st false //
            |false ->
                forcePrint "yo"
                match lst with
                |x::xs ->
                    forcePrint "Whut"
                    aux xs
                |[] ->
                    forcePrint "it was not possible to make a word so therefor we need to use another tile \n"
                    let newList = firstStepFunction chooseTile st pieces
                    makeInput [] newList pieces st false //
        aux myMoveList
     
     
     
     let makeMove (dict: Dict) (st: State.state) (pieces: Map<uint32,tile>) (myMoveList: (uint32*bool) list) =
        List.fold(fun acc i ->
            //go through the dictionary to see if there is a path using step function
            let pathExist = step (getCharFromId pieces i) dict
            match pathExist with
            | Some (b,d) ->
                match b with
                |true ->
                    forcePrint  "a word ends here \n"
                    acc @ [i, b] //a word ends here we add it to myMovesList
                |false ->
                    forcePrint "no word ends here but there is a path \n"
                    acc @ [i, b] //no word ends here we add it to myMovesList
            |None ->
                forcePrint (sprintf "there exists no path to the char: %c \n" (getCharFromId pieces i))
                acc //no path here so we do not add it to the list
            ) myMoveList (getListIdsFromHand st)
        
    *)