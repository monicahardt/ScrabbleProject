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

let rec makeHorizontalCoords (i: int) (startCoord: coord) (wordLength: int) (acc: coord list) : (coord list) =
    if i < wordLength then  
        let newCoord = ((fst startCoord) + i , (snd startCoord))
        let newAcc = newCoord :: acc
        makeHorizontalCoords (i+1) startCoord wordLength newAcc
    else 
        let coordsToReturn = List.rev(acc)
        debugPrint "\n*** THE LIST OF COORDS MADE ***\n"
        List.fold(fun acc coord -> debugPrint (sprintf "x: %d y: %d\n" (fst coord) (snd coord))) () coordsToReturn
        coordsToReturn
let rec makeVerticalCoords (i: int) (startCoord: coord) (wordLength: int) (acc: coord list) : (coord list) =
    if i < wordLength then  
        let newCoord = ((fst startCoord), (snd startCoord) + i)
        let newAcc = newCoord :: acc
        makeVerticalCoords (i+1) startCoord wordLength newAcc
    else 
        let coordsToReturn = List.rev(acc)
        debugPrint "\n*** THE LIST OF COORDS MADE ***\n"
        List.fold(fun acc coord -> debugPrint (sprintf "x: %d y: %d\n" (fst coord) (snd coord))) () coordsToReturn
        coordsToReturn

let makeCoords (startCoord: coord) (direction: Direction) (wordLength: int) =
    match direction with
    |Horizontal -> makeHorizontalCoords 0 startCoord wordLength []
    |Vertical -> makeVerticalCoords 0 startCoord wordLength []

let roomToLeft (st: State.state) (pos: coord)= 
    let occS = st.occupiedSquares
    match Map.tryFind ((fst pos)-1, (snd pos)) occS with
    |Some x -> false
    |None -> true

let roomToRight (st: State.state) (pos: coord)= 
    let occS = st.occupiedSquares
    match Map.tryFind ((fst pos)+1, (snd pos)) occS with
    |Some x -> false
    |None -> true

let roomUp (st: State.state) (pos: coord)= 
    let occS = st.occupiedSquares
    match Map.tryFind ((fst pos), (snd pos)-1) occS with
    |Some x -> false
    |None -> true

let roomDown (st: State.state) (pos: coord)= 
    let occS = st.occupiedSquares
    match Map.tryFind ((fst pos), (snd pos)+1) occS with
    |Some x -> false
    |None -> true


//we only play right and down
let validateWordWithBoard (word: uint32 list) (startPos: coord) (pieces: Map<uint32, tile>) (st: State.state) = 
    let (wordString: string) = makeAWord word pieces
    let charArray = wordString.ToCharArray()
    debugPrint (sprintf "\nThe word we want to place is: %s\n" wordString)

    //loop through the char array
    let rec placeCharOnBoard (i: int) (arr: char array) (pos: coord) (acc: (bool*Direction)) : (bool * Direction) = 
        if i < arr.Length then
            let charToPlace = arr.[i]

            let checkIfDownIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
                match roomUp st pos with 
                    |true -> 
                            debugPrint (sprintf "There was room up from the coord: x: %d y: %d \n" (fst pos) (snd pos))//there was room up so we want to play down (vertically)
                            match roomDown st pos with 
                            |true -> //there was room down so we want to check the left and right from the down space
                                    debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                    let downCoord = (((fst pos)), (snd pos) + 1)
                                    match roomToLeft st downCoord with
                                    |true -> 
                                            debugPrint (sprintf "There was room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                            match roomDown st downCoord with 
                                            |true -> //there was room enough down
                                                    match roomToRight st downCoord with
                                                    |true -> 
                                                            debugPrint (sprintf "There was room right to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                            //we can place tile downwards
                                                            debugPrint "\nThe tile could be placed down\n"
                                                            placeCharOnBoard (i+1) arr.[1..] downCoord (true, Vertical)
                                                    |false -> 
                                                        debugPrint (sprintf "There was not room right to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                        (false, Vertical) //we could not play down
                                            |false -> 
                                                debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                (false, Vertical)
                                    |false -> 
                                        debugPrint (sprintf "There was not room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                        (false, Vertical)
                            |false -> 
                                debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))//check one more time if there is space to down
                                (false, Vertical)
                    |false -> 
                        debugPrint (sprintf "There was not room up from the coord: x: %d y: %d. Aka we cannot place the letter at all \n" (fst pos) (snd pos))//check one more time if there is space to down
                        (false, Vertical) //there was both left and up we cannot play here

            let checkIfRightIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
            //check left on board if there is not room left we want to check down
                match roomToLeft st pos with 
                |true -> //there was room to the left so we want to place the file char
                        debugPrint (sprintf "There was room to the left of the coord: x: %d y: %d \n" (fst pos) (snd pos))
                        match roomToRight st pos with 
                        |true -> //check one more time if there is space to the right
                                debugPrint (sprintf "There was room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                let rightCoord = (((fst pos) + 1), (snd pos))
                                match roomUp st rightCoord with 
                                |true -> 
                                    debugPrint (sprintf "There was room up from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                    match roomToRight st rightCoord with
                                    |true -> //we can place tile to the right
                                        debugPrint (sprintf "There was room to the right of the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        debugPrint "\nThe tile could be placed to the right\n"
                                        placeCharOnBoard (i+1) arr.[1..] rightCoord (true, Horizontal)
                                    |false -> 
                                        debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        //(false, Horizontal) //we could not play right
                                        checkIfDownIsPossible st pos acc
                                |false -> 
                                    debugPrint (sprintf "There was not room up the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                    //(false, Horizontal)
                                    checkIfDownIsPossible st pos acc
                        |false ->  
                            debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))//there was room up from the right coord
                            //(false, Horizontal)
                            checkIfDownIsPossible st pos acc
                |false -> 
                    debugPrint (sprintf "There was not room to the left of the coord: x: %d y: %d. Aka no room to play right, we now check for down \n" (fst pos) (snd pos))//there was not room to the left we want to check up no
                    checkIfDownIsPossible st pos acc
            checkIfRightIsPossible st pos acc
        else 
            acc

    placeCharOnBoard 0 charArray startPos (false, Vertical)


    //i starting at 1 because the first letter i already on the board. The starting position should therefore be
    //the coordinate belonging to the tile on the board


    //here we want to check with the board if it is possible to make a word
   

let test  (pieces: Map<uint32, tile>) (st: State.state) = 
    debugPrint "\n***** TEST WAS CALLED *****\n"
    let testOccS1 = Map.add (0,0) (13u, ('h',1))  st.occupiedSquares
    let testOccS2 = Map.add (0,1) (5u, ('e',1))  testOccS1
    let testOccS3 = Map.add (0,2) (10u, ('j',1))  testOccS2
    let testOccS4 = Map.add (-1,2) (5u, ('e',1))  testOccS3
    let testOccS5 = Map.add (1,0) (5u, ('i',1))  testOccS4

    let st' = {st with occupiedSquares = testOccS5}
    let word = [14u; 15u]
    let startCoord = (-1,2)

    match validateWordWithBoard word startCoord pieces st' with
    |(true,Vertical) -> 
            debugPrint "\nit was possible to place the word in the direction\n"
            makeCoords startCoord Vertical word.Length
    |(true,Horizontal) -> 
            debugPrint "\nit was possible to place the word in the direction\n"
            makeCoords startCoord Horizontal word.Length
    |(false,Vertical) -> 
            debugPrint "\nit was impossible to place the word\n"
            []
    |(false,Horizontal) -> 
            debugPrint "\nit was impossible to place the word\n"
            []



let rec firstAux (st: State.state) (currentHand: MultiSet<uint32>) (currentDict: Dict) (direction: Direction) (pos: coord) (tempWord: uint32 list) (longestWord: uint32 list) (pieces: Map<uint32, tile>) = 
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
                let word = firstAux st myHand d direction startPos [nowChar] [nowChar] pieces
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
        //loop through startingpositions
        first st pieces st.board.center Horizontal