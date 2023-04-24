namespace hej

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
    
    let getCharFromId (pieces: Map<uint32,tile>) (id: uint32) =
        let tile = Map.find id pieces
        (fst (List.head (Set.toList tile)))
   
    let getListOfCharsFromHand (pieces: Map<uint32,tile>)  (st: State.state) =
        st.hand |> MultiSet.fold(fun lst id _ -> lst @ [getCharFromId pieces id]) []
   
    let getListIdsFromHand (st: State.state) =
        toList st.hand
    



    let firstMove (st: State.state) (pieces: Map<uint32,tile>) = 
        let handList = getListIdsFromHand st

        let rec aux (i: int)  =  
            let handWithoutCharWeAreAt = removeSingle handList.[i] st.hand
            let firstCharStepped = step (getCharFromId pieces handList.[i]) st.dict
            let result = []

            match firstCharStepped with
                |Some(b,d) -> 
                    result @ [handList.[i]]
                    let rec lookForWord (dictionary: Dict) (MS: MultiSet<uint32> ) =
                        MultiSet.fold(fun acc id _ ->
                            match (step (getCharFromId pieces id) dictionary) with
                            |Some(b,dict) ->
                                acc @ [id]
                                let updatedHand = removeSingle id handWithoutCharWeAreAt
                                lookForWord dict updatedHand
                                
                            |None -> acc
                        
                            ) result MS
                            
                    lookForWord d handWithoutCharWeAreAt
                        
                |None -> []


            aux i+1

        aux 0                                      
            







        aux 0
    
    let rec makeInputCharList (acc:char list) (myMoveList: (uint32*bool) list) (pieces: Map<uint32,tile>) (st: State.state) (wordFound: bool) =
        match myMoveList with
        |x::xs ->
            match snd(myMoveList.[0]) with
            |true -> //a word ends here
                match wordFound with
                |true ->
                    forcePrint (sprintf "a word ends here and an earlier word acc is now: %d \n" acc.Length)
                    acc //a word has already been found we want to do nothing for now
                |false ->
                    
                    //a word was not found earlier so we now update wordFound to be true
                    let newCharList = acc @ [getCharFromId pieces (fst(myMoveList.[0]))]
                    forcePrint (sprintf "a word ends here but no earlier word acc is now: %d \n" newCharList.Length)
                    makeInputCharList newCharList myMoveList.[1..] pieces st true                                    
            |false -> //no word ends here
                match wordFound with
                |true ->
                    forcePrint (sprintf "no word ends here and an earlier word acc is now: %d \n" acc.Length)
                    acc //a word has already been found we want to do nothing for now
                |false ->
                    
                    let newCharList = acc @ [getCharFromId pieces (fst(myMoveList.[0]))]
                    forcePrint (sprintf "no word ends here and no earlier word acc is now: %d \n" newCharList.Length)
                    makeInputCharList newCharList myMoveList.[1..] pieces st false 
        |[] -> acc
     
    let makeMove1 (firstDict: Dict) (st: State.state) (pieces: Map<uint32,tile>) (myMoveList: (uint32*bool) list) =
        let myHand = st.hand
        let mutable isDone = false
        let rec aux acc dict hand =
            if isDone then acc else 
                MultiSet.fold(fun acc id numberOf ->
                   //is there  a path to the char with the id we are at now?
                   let pathExist = step (getCharFromId pieces id) dict
                   match pathExist with
                   |Some(b,d) ->
                       forcePrint (sprintf "there exists a path to the char: %c \n" (getCharFromId pieces id))
                       match b with
                       |true ->
                           isDone <- true
                           forcePrint  (sprintf "a word ends here %b \n" isDone)
                           acc @ [id, b] //a word ends here we add it to myMovesList and we are done?
                        |false ->
                           forcePrint "no word ends here but there is a path \n"
                           let newHand = removeSingle id myHand //removes the used tile from the hand
                           let newAcc = acc @ [id, b] //no word ends here we add it to myMovesList since there is a path
                           aux newAcc d newHand //we have removed the used one, but we have to add more chars to make a word
                    |None ->
                       forcePrint (sprintf "there exists no path to the char: %c \n" (getCharFromId pieces id))
                       acc //no path here so we do not add it to the list we send the accumulator to the next fold
                ) acc hand
        aux myMoveList firstDict myHand
        
        
    (* Add the tile found on the bord to the list myMove *)          
    let firstStepFunction (id: uint32) (st: State.state) (pieces: Map<uint32,tile>) =
        forcePrint "firstStepFunction was called \n"
        let myMoveList: (uint32 * bool) list = [(id, false)]
        let firstStep = step (getCharFromId pieces id) st.dict //doing the first step to get the dict under the first char
        match firstStep with
        |Some (_,d) ->
            forcePrint (sprintf "first char added to the list was %c \n" (getCharFromId pieces id))
            makeMove1 d st pieces myMoveList
        |None ->
            forcePrint "some unexplainable shit happened \n"
            makeMove1 st.dict st pieces myMoveList //this should never be able to happen
    
    let chooseTile : uint32 = 14u
   
    
    //check if it was possible to make a word with the tile we are at now
    let checkMove1 (myMoveList: (uint32*bool) list) =
        
        let rec aux (lst: (uint32*bool) list) =
            if lst.Length = 0 then
                forcePrint "it was impossible to make a word and therefor we make an input with this \n"
                forcePrint (sprintf "we get here length of the list is %d \n" (lst.[1..]).Length)
                false
                else 
                match snd(lst.[0]) with
                |true ->
                    forcePrint "it was possible to make a word and therefor we make an input with this \n"
                    true
                |false ->
                    aux lst.[1..]
        aux myMoveList
        
   
     
   
module Scrabble =
    open System.Threading
    open Algorithm

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            
            //testing shit
       
            let myFirstList = firstStepFunction 20u st pieces
                
            //List.fold(fun _ (id,bool) -> forcePrint (sprintf "%d %b \n" id bool ) ) () myFirstList
            let testBool = checkMove1 myFirstList
          
                
            let result = makeInputCharList [] myFirstList pieces st false
            
            List.fold(fun _ c -> forcePrint (string c)) () result
              
            
          
            
            (* OLD TESTING
            checkMove tester pieces st |>
            List.fold(fun _ ch -> forcePrint (sprintf "%c" ch) ) ()
            *)
            
            //forcePrint (Print.printTiles pieces) //printing all tiles in the game
          
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
    (*
     let removeSingle a (Multi s) =
        let found = s.TryFind a
        match found with
        |None -> (Multi s)
        |_ -> remove a 1u (Multi s)
        *)

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
        let (testHandList: (uint32 * uint32) list) = [(14u, 1u); (15u, 1u)]
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