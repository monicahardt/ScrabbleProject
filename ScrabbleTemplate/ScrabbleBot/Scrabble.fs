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
    
    let rec firstAux (st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: int) (word: uint32 list) (pieces: Map<uint32, tile>) = 
            let currentHandList = getHandAsList currentHand
            //debugPrint (sprintf "\nsize of the hand is now: %d \n" currentHandList.Length)
            List.iter(fun id -> debugPrint (id.ToString() + "\n")) currentHandList
          

            match currentHandList with 
            |[] -> 
                //debugPrint "\nTHE HAND WAS EMPTY \n"
                //debugPrint "We are done looping the hand"
                //checking if what we found is a valid word
                match lookup (makeAWord (List.rev word) pieces) st.dict with
                |true -> 
                    debugPrint "\nEmpty hand found a word\n"
                    (List.rev word)
                |_ -> 
                    debugPrint "\n Empty hand found no word\n"
                    []
                
            |x::xs ->

                debugPrint "\nTHE HAND WAS NOT EMPTY \n"
                //debugPrint "We are not done looping the hand"
                //try stepping the first in the list at hand
                match nextDict currentDict x pieces with 
                |Some(b,d) ->
                    //debugPrint (sprintf "Stepped the char: %d there was a path" x)
                    if b then
                        let doneWord = x :: (word)
                        debugPrint "\n** MADE A WORD **\n"
                        //firstAux st empty d direction pos doneWord pieces
                         //a word ended
                        (List.rev doneWord)
                    else 
                        //debugPrint "There was a path but no word ended"
                        let newWord = x :: (word) //added the id to the back of the list word
                        let updatedHand = List.fold(fun acc id -> 
                                                                                        removeSingle id acc) st.hand newWord
                        //let updatedHand = removeSingle x currentHand
                        firstAux st updatedHand d direction pos newWord pieces

                |None ->
                    //debugPrint (sprintf "Stepped the char: %d there was not a path" x)
                    let idRemovedFromHand = removeSingle x currentHand //SOMETHING IS WRONG HERE!!!
                    firstAux st idRemovedFromHand currentDict direction pos word pieces
                  
           

    //This is the function mentioned by Jesper
    //Should generate a list of valid moves
    let first (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
        let currentHand = st.hand //our current hand
        let handList = getHandAsList st.hand
        let currentDict = st.dict

        let rec result (i: int) (acc: uint32 list list) (hand: MultiSet<uint32>) = 
            if i > handList.Length-1 
            then 
                acc 
            else 
                let nowChar = handList.[i] 
                let firstStep = nextDict currentDict nowChar pieces
                match firstStep with 
                |Some(b,d) -> 
                    let myHand = removeSingle nowChar currentHand
                    let word = firstAux st myHand d direction i [nowChar] pieces
                    let newAcc = acc @ [word]
                    result (i+1) newAcc currentHand
                |None -> failwith "what went wrong"
        let resultListOfList = result 0 [] st.hand

        debugPrint "\nTHE FIRST WORDS FROM EACH LETTER IN HAND WE CAN PLAY ARE\n"
        List.fold(fun acc lst -> 
                                        debugPrint "\n"
                                        List.iter(fun letter -> debugPrint (sprintf "%c \n" (getCharFromId pieces letter))) lst)
                                        () resultListOfList   
                                                 

    let getNextMoveToPlay (st: State.state) (pieces: Map<uint32,tile>) = 
        match st.occupiedSquares with
        |OC when OC.IsEmpty -> 
            debugPrint "\n ***** FIRST MOVE ***** \n"
            first st pieces st.board.center Horizontal //if the board is empty this is the first move
        |_-> //if the map was not empty there are already placed tiles on the board
            debugPrint "\n ***** NOT FIRST MOVE ***** \n"
            first st pieces st.board.center Horizontal

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
        
 
  