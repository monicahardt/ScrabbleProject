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
        //occupiedSquares : Map<coord, uint32 * (char*int)> //mapping a coordinate to a tuple of (id * tile)
        occupiedSquares : Map<coord, uint32> //mapping a coordinate to a tuple of (id * tile)
        nextTile : ((uint32 * coord) * bool)
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; 
                //occupiedSquares = Map.empty<coord, uint32 * (char*int)>; nextTile = ((100u, (0,0)), true)}
                occupiedSquares = Map.empty; nextTile = ((100u, (0,0)), true)}
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let occupiedSquares st          = st.occupiedSquares
    let nextTile st = st.nextTile

module Algorithm =
    open Dictionary
    
    // ------------ HELPER FUNCTIONS ------------
    
    let getCharFromId (pieces: Map<uint32,tile>) (id: uint32) =
        let tile = Map.find id pieces
        (fst (List.head (Set.toList tile)))
    
    let getPointValueFromId (pieces: Map<uint32,tile>) (id: uint32) =
        let tile = Map.find id pieces
        (snd (List.head (Set.toList tile)))
   
    let getTileFromListIds (pieces: Map<uint32, tile>) (listOfIds: uint32 list) =
        List.fold (fun (acc: string list) id -> 
                    acc @ [(id.ToString() + (string (getCharFromId pieces id)) + (string ((getPointValueFromId pieces id))))]
                    ) [] listOfIds

    let getListOfCharsFromHand (pieces: Map<uint32,tile>)  (st: State.state) =
        st.hand |> MultiSet.fold(fun lst id _ -> lst @ [getCharFromId pieces id]) []
   
    let getListIdsFromHand (st: State.state) =
        toList st.hand

    let removeTileFromHand (st: State.state) (id: uint32) =
        { st with hand = MultiSet.removeSingle id st.hand }

    let removeTilesFromHand (st: State.state) (ids: List<uint32>) = 
        List.fold removeTileFromHand st ids


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

    let nextDict (currentDict: Dict) (id: uint32) (pieces: Map<uint32, tile>) = step (getCharFromId pieces id) currentDict

    let makeAWord (word: uint32 list) (pieces: Map<uint32, tile>): string = 
        List.fold(fun acc id -> 
                                                acc + (string (getCharFromId pieces id))) "" word

    //looping the entire hand with a function


    let getHandAsList (hand: MultiSet<uint32>) =
        toList hand 

    //should return a single word in the form of a list
    //tried continuations
    (*
    let aux(st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: coord) (word: uint32 list) (pieces: Map<uint32, tile>) = 
        let rec firstAux (st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: coord) (word: uint32 list) (pieces: Map<uint32, tile>) c = 
                let currentHandList = getHandAsList currentHand 
                
                match currentHandList with 
                |[] -> 
                    debugPrint "\nTHE HAND WAS EMPTY \n"
                    //debugPrint "We are done looping the hand"
                    //checking if what we found is a valid word
                    match lookup (makeAWord word pieces) st.dict with
                    |true -> c word
                    |_ -> c []
                    
                |x::xs ->
                    debugPrint "\nTHE HAND WAS NOT EMPTY \n"
                    //debugPrint "We are not done looping the hand"
                    //try stepping the first in the list at hand
                    match nextDict currentDict x pieces with 
                    |Some(b,d) ->
                        //debugPrint (sprintf "Stepped the char: %d there was a path" x)
                        if b && (word.Length > 2) then
                            let doneWord = x :: (word)
                            c doneWord //a word ended
                        else 
                            //let newWord = x :: (word) //added the id to the back of the list word
                            let updatedHand = removeSingle x currentHand
                            firstAux st updatedHand d direction pos word pieces (fun res -> c(x :: res))

                    |None ->
                        //debugPrint (sprintf "Stepped the char: %d there was not a path" x)
                        let idRemovedFromHand = removeSingle x currentHand 
                        let idAddedLastToHand = add x 1u currentHand
                        firstAux st idAddedLastToHand currentDict direction pos word pieces (fun res -> c(res))
                |>  (fun lst -> List.rev(lst))

        firstAux st currentHand currentDict direction pos word pieces 
    *)
    
    let rec firstAux (st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: coord) (word: uint32 list) (pieces: Map<uint32, tile>) = 
  
            let currentHandList = getHandAsList currentHand 
            
            match currentHandList with 
            |[] -> 
                debugPrint "\nTHE HAND WAS EMPTY \n"
                //debugPrint "We are done looping the hand"
                //checking if what we found is a valid word
                match lookup (makeAWord word pieces) st.dict with
                |true -> word
                |_ -> []
                
            |x::xs ->
                debugPrint "\nTHE HAND WAS NOT EMPTY \n"
                //debugPrint "We are not done looping the hand"
                //try stepping the first in the list at hand
                match nextDict currentDict x pieces with 
                |Some(b,d) ->
                    //debugPrint (sprintf "Stepped the char: %d there was a path" x)
                    if b then
                        let doneWord = x :: (word)
                        doneWord //a word ended
                    else 
                        let newWord = x :: (word) //added the id to the back of the list word
                        let updatedHand = removeSingle x currentHand
                        firstAux st updatedHand d direction pos newWord pieces

                |None ->
                    //debugPrint (sprintf "Stepped the char: %d there was not a path" x)
                    let idRemovedFromHand = removeSingle x currentHand 
                    let idAddedLastToHand = add x 1u currentHand
                    firstAux st idAddedLastToHand currentDict direction pos word pieces
            |>  (fun lst -> List.rev(lst))

    //This is the function mentioned by Jesper
    //Should generate a list of valid moves
    let first (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
        let currentHand = st.hand //our current hand
        let currentDict = st.dict

        
        let resultListOfList = MultiSet.fold(fun acc i num ->
                                                                    let word = firstAux st currentHand currentDict direction startPos [] pieces 
                                                                    acc @ [word]) [] currentHand

    
        List.fold(fun acc lst -> 
                                        debugPrint "\n PRINTING A WORD \n"
                                        List.fold(fun acc id -> debugPrint (sprintf "%c \n" (getCharFromId pieces id))) () lst)
                                        () resultListOfList   
                                                 

    let getNextMoveToPlay (st: State.state) (pieces: Map<uint32,tile>) = 
        match st.occupiedSquares with
        |OC when OC.IsEmpty -> 
            debugPrint "\n ***** FIRST MOVE ***** \n"
            first st pieces st.board.center Horizontal //if the board is empty this is the first move
        |_-> //if the map was not empty there are already placed tiles on the board
            debugPrint "\n ***** NOT FIRST MOVE ***** \n"
            first st pieces st.board.center Horizontal

                                     
    (*
    let findPossibleMovesNew (st: State.state) (pieces: Map<uint32,tile>) (firstDict: Dict) (firstCharId: uint32): Map<uint32, uint32 list> = 
            
            let handIds = getListIdsFromHand st

            //outer recursion should recurse over the hand
            let rec aux (i: int) (auxMap: Map<uint32,uint32 list>) (dictToUse: Dict) = 
                //debugPrint "AUX CALLED"
                if(i >= handIds.Length) 
                then 
                    debugPrint "WE ARE DONE FINDING WORDS"
                    //there we not any tiles left on the hand to go through
                    auxMap 
                else 
                    let charWeAreAt = handIds.[i]
                    debugPrint (sprintf ("aux running again the char is now %c \n" )(getCharFromId pieces handIds.[i])) 
                    debugPrint (sprintf "the char we are checking is %d \n" charWeAreAt) 
                    let handWithoutCharWeAreAt = removeSingle handIds.[i] st.hand
                    let firstCharStepped = step (getCharFromId pieces handIds.[i]) dictToUse
                    
                    match firstCharStepped with
                        |Some(b,d) -> //this should always happen
                            //adding the first id to the list result
                            //debugPrint "Found the first char \n"
                            let map = Map.add charWeAreAt [charWeAreAt] auxMap
                                
                            //recursive function start
                            let rec lookForWord (dictionary: Dict) (MS: MultiSet<uint32>) (listOfPossibleWords: (uint32 list*bool)) =
                                debugPrint "******NEW RECURSION***** \n"
                            
                                //folding over the rest of the multiset
                                MultiSet.fold(fun (acc: (uint32 list * bool)) id _ ->
                                    if (snd acc) then acc else

                                    //debugPrint (sprintf "folding over the char with id %d \n" id)
                                    match (step (getCharFromId pieces id) dictionary) with
                                    |Some(b,dict) ->
                                        //let updatedAcc = (List.rev (id :: (fst (acc)))) //there was a path to this id so we add it to the foldList
                                        let updatedHand = removeSingle id MS
                                        
                                        debugPrint (sprintf "there was a path to the char with id %d \n" id)
                                        debugPrint (sprintf "printing size of the hand we are sending on: %d \n" (size updatedHand))
                                    
                                        if b then debugPrint "a word has ended \n"  else debugPrint "continuing"
                                
                                        //we also return words that are not done, but we do this to make the first work! 
                                        //this should be changed!!!!
                                        let updatedAccList = ((id :: (fst (acc)))) 
                                        lookForWord dict updatedHand ((List.rev(updatedAccList)),b)

                                    |None ->
                                        //debugPrint (sprintf "there was not a path to the char with id %d \n" id)
                                        acc
                                ) listOfPossibleWords MS |> (fun x-> 
                                                                    debugPrint "********A fold is complete. Aka a recursion is done******** \n" 
                                                                    x) 
                            
                            //first call to lookForWord
                            let resultFromLookForWord = lookForWord d handWithoutCharWeAreAt (map.[charWeAreAt], false)
                            //adding to the map the word (list) found in the lookForWord function
                            let map = auxMap |> Map.add charWeAreAt (List.rev (fst resultFromLookForWord))
                            //aux called with i incremented
                            aux (i+1) map d
                                
                        |None -> 
                            debugPrint "We are in the none case. There was not a path to this char"
                            aux (i+1) auxMap dictToUse
                            //failwith "This should not happen"
                            //auxMap
            
            //first call to aux with i being 0 and our map being empty
            if firstCharId = 100u then
                aux 0 Map.empty st.dict //if it was the first move we do as always
            else 
                let firstAuxMap = Map.empty
                let mapToUse = Map.add firstCharId [firstCharId] firstAuxMap
                aux 0 mapToUse firstDict


    //The first function to be called to check if this is the first word we want to play or not
    let firstFunction(st: State.state) (pieces: Map<uint32, tile>) =
        if (fst (fst st.nextTile)) = 100u then 
            debugPrint " \nThis is the first move \n"
            findPossibleMovesNew st pieces st.dict 100u
        else 
            debugPrint "\nThis was not the first move \n"
            let lastIdOnBoard = fst(fst st.nextTile)
            let firstChar = getCharFromId pieces lastIdOnBoard
            debugPrint (sprintf "\nThe char on the board we are looking at is %c\n" firstChar)
            let firstStep = step firstChar st.dict
            match firstStep with 
            |Some(b,dict) -> 
                findPossibleMovesNew st pieces dict lastIdOnBoard
            |None -> failwith "Some wierd shit happened"
            
            

// ------------ MOVE ------------

    //validates the words to possible play on the board
    let validateMove (st: State.state) (pieces: Map<uint32,tile>)  (resultMap: Map<uint32, uint32 list>)= 
         Map.fold(fun acc id lst -> 
                List.fold(fun (accS: string) (id: uint32) -> 
                    //we call loopkup to check if a word is valid
                    accS + (string (getCharFromId pieces id))
                ) "" lst |> 
                (fun str ->
                    match (lookup str st.dict) with
                    |true -> acc
                    |false -> Map.remove(id) acc)
            ) resultMap resultMap           


    //Takes the first word in the map of valid moves and creates a list of ids to play
    let createAWordsList (st: State.state) (pieces: Map<uint32,tile>) (validMoves: Map<uint32, uint32 list>)   = 
        let board = st.occupiedSquares
        if board.IsEmpty then
             debugPrint "WE ARE STUCK IN BOARD IS EMPTY"
             Map.fold(fun (acc: (uint32 list) list) id (lst: uint32 list) -> 
                if lst.Length > 2 then 
                    [lst] @ acc
                else acc
             ) List.Empty validMoves |> (fun listOfLongerWords -> 
                    if listOfLongerWords.IsEmpty then [] else 
                        List.fold(fun acc id -> (id :: acc)) [] (listOfLongerWords.Head)
                        )                                     
        else
            debugPrint "WE ARE STUCK IN MAKING A LIST OF WORDS"
            if (not (validMoves.IsEmpty)) then
                let word = Map.minKeyValue(validMoves)
                List.fold(fun acc id -> (id :: acc)) [] (snd (word))
            else 
                debugPrint "The list of valid moves was empty"
                []
        |> (fun lstToFlip -> List.rev(lstToFlip))


    //Takes a list of coordinates and a list of tiles and creates the input string
    let createFinalInputString (coords: (int * int) list) (tiles: string list) =
        let rec addCoordAndTile (coord: (int * int) list) (tile: string list) (acc: string)=
            match (coord, tile) with
            | (c::cs, t::ts) -> 
                let newString = "(" + (string (fst c)) + " " + (string (snd c)) + " " + t + ") " 
                let newAcc = acc + newString
                addCoordAndTile cs ts newAcc
            | _ -> acc
        addCoordAndTile coords tiles ""
*)
   
module Scrabble =
    open System.Threading
    open Algorithm

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            debugPrint "\n AUX WAS CALLED NEW ROUND \n" 
            Print.printHand pieces (State.hand st)

            getNextMoveToPlay st pieces
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
        
 
  