module internal Moves

open ScrabbleUtil.Dictionary
open ScrabbleUtil.DebugPrint
open ScrabbleUtil
open MultiSet
open MoveUtilities
open Hand
open Tile


type Direction =
| Horizontal
| Vertical


let rec firstAux (st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: int) (tempWord: uint32 list) (longestWord: uint32 list) (pieces: Map<uint32, tile>) = 
        let currentHandList = getHandAsList currentHand
        //debugPrint (sprintf "\nsize of the hand is now: %d \n" currentHandList.Length)
        // List.iter(fun id -> debugPrint (id.ToString() + "\n")) currentHandList
        match currentHandList with 
        |[] -> 
            //debugPrint "\nTHE HAND WAS EMPTY \n"
            //debugPrint "We are done looping the hand"
            //checking if what we found is a valid word
            match lookup (makeAWord (List.rev longestWord) pieces) st.dict with
            |true -> 
                //debugPrint "\nEmpty hand found a word\n"
                (List.rev longestWord)
            |_ -> 
                //debugPrint "\n Empty hand found no word\n"
                []
            
        |x::xs ->
            //debugPrint "\nTHE HAND WAS NOT EMPTY \n"
            //debugPrint "We are not done looping the hand"
            //try stepping the first in the list at hand
            match nextDict currentDict x pieces with 
            |Some(b,d) ->
                //debugPrint (sprintf "Stepped the char: %d there was a path" x)
                if b then
                    //debugPrint "\n** MADE A WORD **\n"
                    let newTempWord = x :: (tempWord)
                    let newHand = removeSingle x currentHand

                    if(newTempWord.Length > longestWord.Length) then
                        //debugPrint "\n** FOUND A LONGER WORD THAN WE ALREADY HAVE **\n"
                        let newLongestWord = x :: (longestWord)
                        firstAux st newHand d direction pos newTempWord newLongestWord pieces
                    else 
                        //debugPrint "\n** DID NOT FIND A LONGER WORD THAN WE ALREADY HAVE **\n"
                        firstAux st newHand d direction pos newTempWord longestWord pieces
                else 
                    //debugPrint "There was a path but no word ended"
                    let longestWord = x :: (longestWord) //added the id to the back of the list word
                    let updatedHand = List.fold(fun acc id -> 
                                                                                    removeSingle id acc) st.hand longestWord
                    //let updatedHand = removeSingle x currentHand
                    firstAux st updatedHand d direction pos longestWord longestWord pieces

            |None ->
                //debugPrint (sprintf "Stepped the char: %d there was not a path" x)
                let idRemovedFromHand = removeSingle x currentHand //SOMETHING IS WRONG HERE!!!
                firstAux st idRemovedFromHand currentDict direction pos longestWord longestWord pieces
                
//This is the function mentioned by Jesper
//Should generate a list of valid moves
let first (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
    let currentHand = st.hand //our current hand
    let handList = getHandAsList st.hand
    let currentDict = st.dict

    //looping our hand
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
                let word = firstAux st myHand d direction i [nowChar] [nowChar] pieces
                let newAcc = acc @ [word]
                result (i+1) newAcc currentHand
            |None -> failwith "what went wrong"
    let resultListOfList = result 0 [] st.hand

    debugPrint "\nTHE FIRST WORDS FROM EACH LETTER IN HAND WE CAN PLAY ARE\n"
    List.fold(fun acc lst -> 
                                    debugPrint "\n"
                                    List.iter(fun letter -> debugPrint (sprintf "%c \n" (getCharFromId pieces letter))) lst)
                                    () resultListOfList   

    resultListOfList                                     
                                   

let getNextMoveToPlay (st: State.state) (pieces: Map<uint32,tile>) = 
    match st.occupiedSquares with
    |OC when OC.IsEmpty -> 
        debugPrint "\n ***** FIRST MOVE ***** \n"
        first st pieces st.board.center Horizontal //if the board is empty this is the first move
    |_-> //if the map was not empty there are already placed tiles on the board
        debugPrint "\n ***** NOT FIRST MOVE ***** \n"
        first st pieces st.board.center Horizontal