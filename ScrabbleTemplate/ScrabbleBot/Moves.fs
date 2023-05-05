module internal Moves

open ScrabbleUtil.Dictionary
open ScrabbleUtil.DebugPrint
open ScrabbleUtil
open MultiSet
open MoveUtilities


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
        coordsToReturn
let rec makeVerticalCoords (i: int) (startCoord: coord) (wordLength: int) (acc: coord list) : (coord list) =
    if i < wordLength then  
        let newCoord = ((fst startCoord), (snd startCoord) + i)
        let newAcc = newCoord :: acc
        makeVerticalCoords (i+1) startCoord wordLength newAcc
    else 
        let coordsToReturn = List.rev(acc)
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

let roomDown (st: State.state) (pos: coord) = 
    let occS = st.occupiedSquares
    match Map.tryFind ((fst pos), (snd pos)+1) occS with
    |Some x -> false
    |None -> true


(* Validating if a word can be placed on the board
We always try to play to the right first. Here there are two cases. If we have already placed a letter in the word we are 
trying to place to the right, we want to skip the first left check. Because then we would hit the letter from the word 
we are placing. Therefore we use the boolean firstCharPlacedRight to make sure this is taken care of. 

If we were unable to play right we try to play down. The same goes with the boolean if we have already placed a letter going down
we want to skip the first check up. 
*)
let validateWordWithBoard (word: uint32 list) (startPos: coord) (pieces: Map<uint32, tile>) (st: State.state) = 
    let (wordString: string) = makeAWord word pieces
    let charArray = wordString.ToCharArray()

    //loop through the char array
    let rec placeCharOnBoard (i: int) (startArr: char array) (arr: char array) (startPos: coord) (pos: coord) (acc: (bool*Direction)) (firstCharPlacedRight: bool) (firstCharPlacedDown: bool) : (bool * Direction) = 
        if arr.Length > 0 then
            let checkIfDownIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
                if firstCharPlacedDown then 
                    match roomDown st pos with 
                                |true -> //there was room down so we want to check the left and right from the down space
                                        let downCoord = (((fst pos)), (snd pos) + 1)
                                        match roomToLeft st downCoord with
                                        |true -> 
                                                match roomDown st downCoord with 
                                                |true -> //there was room enough down
                                                        match roomToRight st downCoord with
                                                        |true -> 
                                                            //we can place tile downwards
                                                            placeCharOnBoard (i+1) startArr arr.[1..] downCoord downCoord (true, Vertical) false true
                                                        |false -> 
                                                            (false, Vertical) //we could not play down
                                                |false -> 
                                                    (false, Vertical)
                                        |false -> 
                                            (false, Vertical)
                                |false -> 
                                    (false, Vertical)
                else 
                    match roomUp st pos with 
                        |true ->
                                match roomDown st pos with 
                                |true -> //there was room down so we want to check the left and right from the down space
                                        let downCoord = (((fst pos)), (snd pos) + 1)
                                        match roomToLeft st downCoord with
                                        |true -> 
                                                match roomDown st downCoord with 
                                                |true -> //there was room enough down
                                                        match roomToRight st downCoord with
                                                        |true -> //we can place tile downwards
                                                            placeCharOnBoard (i+1) startArr arr.[1..] downCoord downCoord (true, Vertical) false true
                                                        |false -> 
                                                            (false, Vertical) //we could not play down
                                                |false -> 
                                                    (false, Vertical)
                                        |false -> 
                                            (false, Vertical)
                                |false -> 
                                    (false, Vertical)
                        |false -> 
                            (false, Vertical) //there was both left and up we cannot play here

            let checkIfRightIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
            //check left on board if there is not room left we want to check down
                if firstCharPlacedRight then
                    match roomToRight st pos with 
                            |true -> //check one more time if there is space to the right
                                    let rightCoord = (((fst pos) + 1), (snd pos))
                                    match roomUp st rightCoord with 
                                    |true -> 
                                        match roomDown st rightCoord with
                                        |true ->
                                            match roomToRight st rightCoord with
                                            |true -> //we can place tile to the right  
                                                placeCharOnBoard (i+1) startArr arr.[1..] startPos rightCoord (true, Horizontal) true false
                                            |false -> 
                                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                        |false -> 
                                            placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                    |false -> 
                                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                            |false ->  
                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false


                else
                    match roomToLeft st pos with 
                    |true -> //there was room to the left so we want to place the file char
                            match roomToRight st pos with 
                            |true -> //check one more time if there is space to the right
                                    let rightCoord = (((fst pos) + 1), (snd pos))
                                    match roomUp st rightCoord with 
                                    |true -> 
                                        match roomDown st rightCoord with
                                        |true -> 
                                            match roomToRight st rightCoord with
                                            |true -> //we can place tile to the right
                                                placeCharOnBoard (i+1) startArr arr.[1..] startPos rightCoord (true, Horizontal) true false
                                            |false -> 
                                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                        |false -> 
                                            placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                    |false -> 
                                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                            |false ->  
                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                    |false -> 
                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
            
            match snd acc with
            |Horizontal -> checkIfRightIsPossible st pos acc 
            |Vertical -> checkIfDownIsPossible st pos acc
        else 
            acc

    placeCharOnBoard 0 charArray charArray startPos startPos (false, Horizontal) false false //we always want to check if we can play right first, since this function calls the left check if it was not possible


(* Validating all words found in stepToRest with lookup first and then try to place the word on the board *)
(* This function returns only words in dictionary that can be placed on the board *)
let validateWordsFound (st: State.state) (pieces: Map<uint32, tile>) (wordsFound: uint32 list list) (pos: coord) = 
    List.fold(fun (acc: (uint32 list * coord list)) word -> 
                //check if the word can be placed
                let myWord = makeAWord (List.rev word) pieces
                match lookup myWord st.dict with
                |true -> 
                    //debugPrint (sprintf "\n MADE A WORD: %s \n" myWord)
                    let possibleCoords = 
                         match validateWordWithBoard (List.rev word) pos pieces st with 
                             |(true,Vertical) -> 
                                 //debugPrint "\nit was possible to place the word in the direction\n"
                                 makeCoords pos Vertical word.Length
                             |(true,Horizontal) -> 
                                 //debugPrint "\nit was possible to place the word in the direction\n"
                                 makeCoords pos Horizontal word.Length
                             |(false,_) -> []

                    match possibleCoords with
                    |[] ->  
                        //it was not possible to place the word on the board
                        acc
                      
                    |_ -> //it was possible so we check if this word is longer than the current longest word
                        if word.Length > (fst acc).Length then 
                            ((List.rev word), possibleCoords) 
                        else 
                            acc
                    
                |false -> acc

                ) ([], []) wordsFound

(* Stepping all possible char combincation on the hand given *)
let rec stepToRest (st: State.state) (hand: MultiSet<uint32>) (dict: Dict) (wordToBuild: uint32 list) (pieces: Map<uint32, tile>) (outerAcc: uint32 list list) (pos: coord)= 
        //folding over the hand given
        fold(fun acc id _ -> 
            match nextDict dict id pieces with 
            |Some(b,d) -> //there was a path
                let updatedWord = id :: wordToBuild
                //updating the hand to be the hand without the char stepped to avoid using it again
                let updatedHand = removeSingle id hand
                if b then
                    //if a word ended we add that word to the accumulator which is a list of words
                    let updatedAcc = updatedWord :: acc
                    stepToRest st updatedHand d updatedWord pieces updatedAcc pos
                else
                    stepToRest st updatedHand d updatedWord pieces acc pos
            |None -> acc
                //there was not a path
        ) outerAcc hand

                
(* Function to loop through hand and try to make a first word to play on the board *)
let first (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
    let currentHand = st.hand
    let handList = getHandAsList st.hand
    let currentDict = st.dict

    //looping our hand
    let rec result (i: int) (acc: Map<uint32 list, coord list> ) = 
        if i > handList.Length-1 
        then 
            acc 
        else 
            // check for wildcard, hardcode it to an A     
            let nowChar = handList.[i] 
            let firstStep = 
                if handList.[i] = 0u then 
                    nextDict currentDict 1u pieces
                else 
                    nextDict currentDict nowChar pieces

            match firstStep with 
            |Some(b,d) -> 
                //the hand without the char already stepped to avoid using it twice in a word
                let myHand = removeSingle nowChar currentHand
                let wordsInListOfList = stepToRest st myHand d [nowChar] pieces [] startPos
                let (word, coords) = validateWordsFound st pieces wordsInListOfList startPos
                let newAcc = Map.add word coords acc
                result (i+1) newAcc

            |None -> failwith "what went wrong"
    let resultMap = result 0 Map.empty 
    resultMap                                    
                                   
(* Function to loop through all anchorpoints *)
let second (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
    let currentHand = st.hand
    let currentDict = st.dict
    let allOccupiedSqaures = st.occupiedSquares
    let occSquaresList = Map.toList allOccupiedSqaures 

    //looping all tiles placed on board, try to make words using them as first letter in a word combined with our tiles on hand
    let rec result (i: int) (acc: Map<uint32 list, coord list> ) (longestWordPlaceable: uint32 list) = 
        if i > occSquaresList.Length-1 
        then 
            acc 
        else 
            let nowChar = if (fst(snd occSquaresList.[i])) = 0u then 1u else (fst(snd occSquaresList.[i]))
            let newStartCoord = fst occSquaresList.[i] 
            let firstStep = nextDict currentDict nowChar pieces
            match firstStep with 
            |Some(b,d) -> 
                let wordsInListOfList = stepToRest st currentHand d [nowChar] pieces [] newStartCoord
                let (word, coords) = validateWordsFound st pieces wordsInListOfList newStartCoord
                if word.IsEmpty then 
                    //we could not make a word with this tile on the board
                    result (i+1) acc longestWordPlaceable
                else 
                    //check if we have a longer placable word to play
                    if word.Length >= longestWordPlaceable.Length then   
                        let newAcc = Map.add word coords acc
                        result (i+1) newAcc word
                    else 
                        result (i+1) acc longestWordPlaceable
            |None -> failwith "what went wrong"
    let resultMap = result 0 Map.empty []
    resultMap                                    

let getNextMoveToPlay (st: State.state) (pieces: Map<uint32,tile>) = 
    match st.occupiedSquares with
    |OC when OC.IsEmpty -> 
        first st pieces st.board.center Horizontal //if the board is empty this is the first move
    |_-> 
        second st pieces st.board.center Horizontal