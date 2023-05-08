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
        //debugPrint "\n*** THE LIST OF COORDS MADE ***\n"
        //List.fold(fun acc coord -> debugPrint (sprintf "x: %d y: %d\n" (fst coord) (snd coord))) () coordsToReturn
        coordsToReturn
let rec makeVerticalCoords (i: int) (startCoord: coord) (wordLength: int) (acc: coord list) : (coord list) =
    if i < wordLength then  
        let newCoord = ((fst startCoord), (snd startCoord) + i)
        let newAcc = newCoord :: acc
        makeVerticalCoords (i+1) startCoord wordLength newAcc
    else 
        let coordsToReturn = List.rev(acc)
        //debugPrint "\n*** THE LIST OF COORDS MADE ***\n"
        //List.fold(fun acc coord -> debugPrint (sprintf "x: %d y: %d\n" (fst coord) (snd coord))) () coordsToReturn
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


//we only play right and down
let validateWordWithBoard (word: uint32 list) (startPos: coord) (pieces: Map<uint32, tile>) (st: State.state) = 
    let (wordString: string) = makeAWord word pieces
    //let wordWeAreValidating = debugPrint (sprintf "\nWORD WE ARE VALIDATING ON BOARD IS: %s\n"  wordString)
    let charArray = wordString.ToCharArray()

    //loop through the char array
    let rec placeCharOnBoard (i: int) (startArr: char array) (arr: char array) (startPos: coord) (pos: coord) (acc: (bool*Direction)) (firstCharPlacedRight: bool) (firstCharPlacedDown: bool) : (bool * Direction) = 
        if arr.Length > 0 then
            let checkIfDownIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
                //debugPrint "*****CHECKING DOWN*****\n"
                if firstCharPlacedDown then 
                    match roomDown st pos with 
                                |true -> //there was room down so we want to check the left and right from the down space
                                        //debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                        let downCoord = (((fst pos)), (snd pos) + 1)
                                        match roomToLeft st downCoord with
                                        |true -> 
                                                //debugPrint (sprintf "There was room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                match roomDown st downCoord with 
                                                |true -> //there was room enough down
                                                        //debugPrint (sprintf "There was room down from to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                        match roomToRight st downCoord with
                                                        |true -> 
                                                            //debugPrint (sprintf "There was room right from the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                            //we can place tile downwards
                                                            //debugPrint "\nThe tile could be placed down\n"
                                                            placeCharOnBoard (i+1) startArr arr.[1..] downCoord downCoord (true, Vertical) false true
                                                        |false -> 
                                                            //debugPrint (sprintf "There was not room right to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                            (false, Vertical) //we could not play down
                                                |false -> 
                                                    //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                    (false, Vertical)
                                        |false -> 
                                            //debugPrint (sprintf "There was not room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                            (false, Vertical)
                                |false -> 
                                    //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))//check one more time if there is space to down
                                    (false, Vertical)
                else 
                    match roomUp st pos with 
                        |true -> 
                                //debugPrint (sprintf "There was room up from the coord: x: %d y: %d \n" (fst pos) (snd pos))//there was room up so we want to play down (vertically)
                                match roomDown st pos with 
                                |true -> //there was room down so we want to check the left and right from the down space
                                        //debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                        let downCoord = (((fst pos)), (snd pos) + 1)
                                        match roomToLeft st downCoord with
                                        |true -> 
                                                //debugPrint (sprintf "There was room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                match roomDown st downCoord with 
                                                |true -> //there was room enough down
                                                        //debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                        match roomToRight st downCoord with
                                                        |true -> //we can place tile downwards
                                                            //debugPrint (sprintf "There was room right to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                            //debugPrint "\nThe tile could be placed down\n"
                                                            placeCharOnBoard (i+1) startArr arr.[1..] downCoord downCoord (true, Vertical) false true
                                                        |false -> 
                                                            //debugPrint (sprintf "There was not room right to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                            (false, Vertical) //we could not play down
                                                |false -> 
                                                    //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                                    (false, Vertical)
                                        |false -> 
                                            //debugPrint (sprintf "There was not room left to the coord: x: %d y: %d \n" (fst downCoord) (snd downCoord))//check one more time if there is space to down
                                            (false, Vertical)
                                |false -> 
                                    //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst pos) (snd pos))//check one more time if there is space to down
                                    (false, Vertical)
                        |false -> 
                            //debugPrint (sprintf "There was not room up from the coord: x: %d y: %d. Aka we cannot place the letter at all \n" (fst pos) (snd pos))//check one more time if there is space to down
                            (false, Vertical) //there was both left and up we cannot play here

            let checkIfRightIsPossible (st: State.state) (pos: coord) (acc: (bool*Direction)) = 
                //debugPrint "*****CHECKING RIGHT*****\n"
            //check left on board if there is not room left we want to check down
                if firstCharPlacedRight then
                    match roomToRight st pos with 
                            |true -> //check one more time if there is space to the right
                                    //debugPrint (sprintf "There was room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                    let rightCoord = (((fst pos) + 1), (snd pos))
                                    match roomUp st rightCoord with 
                                    |true -> 
                                        //debugPrint (sprintf "There was room up from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        match roomDown st rightCoord with
                                        |true ->
                                            //debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//check one more time if there is space to down
                                            match roomToRight st rightCoord with
                                            |true -> //we can place tile to the right  
                                                //debugPrint (sprintf "There was room right from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//check one more time if there is space to down
                                                //debugPrint "\nThe tile could be placed to the right\n"
                                                placeCharOnBoard (i+1) startArr arr.[1..] startPos rightCoord (true, Horizontal) true false
                                            |false -> 
                                                //debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                                //(false, Horizontal) //we could not play right
                                                //checkIfDownIsPossible st pos acc
                                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                        |false -> 
                                            //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                            //(false, Horizontal)
                                            //checkIfDownIsPossible st pos acc 
                                            placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                    |false -> 
                                        //debugPrint (sprintf "There was not room up the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        //(false, Horizontal)
                                        //checkIfDownIsPossible st pos acc 
                                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                            |false ->  
                                //debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))//there was room up from the right coord
                                //(false, Horizontal)
                                //checkIfDownIsPossible st pos acc
                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false


                else
                    match roomToLeft st pos with 
                    |true -> //there was room to the left so we want to place the file char
                            //debugPrint (sprintf "There was room to the left of the coord: x: %d y: %d \n" (fst pos) (snd pos))
                            match roomToRight st pos with 
                            |true -> //check one more time if there is space to the right
                                    //debugPrint (sprintf "There was room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))
                                    let rightCoord = (((fst pos) + 1), (snd pos))
                                    match roomUp st rightCoord with 
                                    |true -> 
                                        //debugPrint (sprintf "There was room up from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        match roomDown st rightCoord with
                                        |true -> 
                                            //debugPrint (sprintf "There was room down from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                            match roomToRight st rightCoord with
                                            |true -> //we can place tile to the right
                                                //debugPrint (sprintf "There was room to the right of the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))
                                                //debugPrint "\nThe tile could be placed to the right\n"
                                                placeCharOnBoard (i+1) startArr arr.[1..] startPos rightCoord (true, Horizontal) true false
                                            |false -> 
                                                //debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                                //(false, Horizontal) //we could not play right
                                                //checkIfDownIsPossible st pos acc 
                                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                        |false -> 
                                            //debugPrint (sprintf "There was not room down from the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                            //checkIfDownIsPossible st pos acc 
                                            placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                                    |false -> 
                                        //debugPrint (sprintf "There was not room up the coord: x: %d y: %d \n" (fst rightCoord) (snd rightCoord))//there was room up from the right coord
                                        //(false, Horizontal)
                                        //checkIfDownIsPossible st pos acc 
                                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                            |false ->  
                                //debugPrint (sprintf "There was not room to the right of the coord: x: %d y: %d \n" (fst pos) (snd pos))//there was room up from the right coord
                                //(false, Horizontal)
                                //checkIfDownIsPossible st pos acc
                                placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
                    |false -> 
                        //debugPrint (sprintf "There was not room to the left of the coord: x: %d y: %d. Aka no room to play right, we now check for down \n" (fst pos) (snd pos))//there was not room to the left we want to check up no
                        //checkIfDownIsPossible st pos acc
                        placeCharOnBoard (i+1) startArr startArr startPos startPos (false, Vertical) false false
            
            match snd acc with
            |Horizontal -> checkIfRightIsPossible st pos acc 
            |Vertical -> checkIfDownIsPossible st pos acc
        else 
            acc

    placeCharOnBoard 0 charArray charArray startPos startPos (false, Horizontal) false false //we always want to check if we can play right first, since this function calls the left check if it was not possible

    //i starting at 1 because the first letter i already on the board. The starting position should therefore be
    //the coordinate belonging to the tile on the board
    //here we want to check with the board if it is possible to make a word

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
                      
                    |_ -> //it was possible so we check if this word is longer than the current longest word *)
                        if word.Length > (fst acc).Length then 
                            ((List.rev word), possibleCoords) 
                        else 
                            acc
                    
                |false -> acc

                ) ([], []) wordsFound

let rec stepToRest (st: State.state) (hand: MultiSet<uint32>) (dict: Dict) (wordToBuild: uint32 list) (pieces: Map<uint32, tile>) (outerAcc: uint32 list list) (pos: coord)= 
        fold(fun acc id numOf -> 
            let word = wordToBuild
            match nextDict dict id pieces with 
            |Some(b,d) -> //there was a path
                let updatedWord = id :: wordToBuild
                let updatedHand = removeSingle id hand
                if b then
                    let updatedAcc = updatedWord :: acc
                    stepToRest st updatedHand d updatedWord pieces updatedAcc pos
                else
                    stepToRest st updatedHand d updatedWord pieces acc pos
            |None -> acc
                //there was not a path
        ) outerAcc hand

                
//This is the function mentioned by Jesper
//Should generate a list of valid moves
let first (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
    //let currentHand = st.hand //our current hand
    //let wildcard = Seq.head (pieces.[0u])
    (*if contains 0u st.hand then removeSingle 0u st.hand else*)
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
                (*if handList.[i] = 0u then removeSingle 0u currentHand else*)
                let myHand = removeSingle nowChar currentHand
                let wordsInListOfList = stepToRest st myHand d [nowChar] pieces [] startPos
                let (word, coords) = validateWordsFound st pieces wordsInListOfList startPos
                //debugPrint (sprintf "\nThe size of the list of long words: %d\n" word.Length)
                //debugPrint (sprintf "\n*************************Printing the longest word found: %s\n" (makeAWord word pieces))
                let newAcc = Map.add word coords acc
                result (i+1) newAcc

            |None -> failwith "what went wrong"
    let resultMap = result 0 Map.empty 

    resultMap                                    
                                   

let second (st: State.state) (pieces: Map<uint32, tile>) (startPos: coord) (direction: Direction)  = 
    //let currentHand = st.hand //our current hand
    (*if contains 0u st.hand then removeSingle 0u st.hand else*)
    let currentHand = st.hand
    let currentDict = st.dict
    let allOccupiedSqaures = st.occupiedSquares
    let occSquaresList = Map.toList allOccupiedSqaures 

    //try to compare the words length that we can place
    //looping the board now
    let rec result (i: int) (acc: Map<uint32 list, coord list> ) (longestWordPlaceable: uint32 list) = 
        if i > occSquaresList.Length-1 
        then 
            acc 
        else 
            let nowChar = if (fst(snd occSquaresList.[i])) = 0u then 
                                //debugPrint "WE GOT A WILDCARD SOMETHING GOES WRONG1"
                                1u else (fst(snd occSquaresList.[i]))

            //debugPrint (sprintf "THE CHAR ON THE BOARD WE ARE LOOKING AT IS: %c \n" (getCharFromId pieces nowChar))
            let newStartCoord = fst occSquaresList.[i] 
            let firstStep = nextDict currentDict nowChar pieces
            match firstStep with 
            |Some(b,d) -> 
                let wordsInListOfList = stepToRest st currentHand d [nowChar] pieces [] newStartCoord
                let (word, coords) = validateWordsFound st pieces wordsInListOfList newStartCoord
                //let (word, coords) = firstAux st currentHand d direction newStartCoord [nowChar] [nowChar] [] pieces
                //if the word is an empty list we cannot make a word with this tile already on the board
                if word.IsEmpty then 
                    //we could not make a word with this tile on the board
                    result (i+1) acc longestWordPlaceable
                else 
                    //play to the right if possible
                    if word.Length >= longestWordPlaceable.Length then   
                        //debugPrint "We found a longer word to play from the board\n"
                        let newAcc = Map.add word coords acc
                        result (i+1) newAcc word
                    else 
                        result (i+1) acc longestWordPlaceable

            |None -> failwith "what went wrong"
    let resultMap = result 0 Map.empty []

    //debugPrint "\nTHE FIRST WORDS FROM EACH LETTER IN HAND WE CAN PLAY ARE\n"
   
   (*
    Map.fold(fun acc k v -> 
                        debugPrint "\n**WORD**\n"
                        List.iter(fun letter -> debugPrint (sprintf "%c \n" (getCharFromId pieces letter))) k
                        debugPrint "\n**Coord**\n"
                        List.iter(fun coord -> debugPrint (sprintf "x: %d y: %d \n" (fst coord) (snd coord))) v
                     
    )() resultMap
    *)
    resultMap                                    
               


let getNextMoveToPlay (st: State.state) (pieces: Map<uint32,tile>) = 
    match st.occupiedSquares with
    |OC when OC.IsEmpty -> 
        //debugPrint "\n ***** FIRST MOVE ***** \n"
        first st pieces st.board.center Horizontal //if the board is empty this is the first move
    |_-> //if the map was not empty there are already placed tiles on the board
        //debugPrint "\n ***** NOT FIRST MOVE ***** \n"
        //loop through startingpositions
        second st pieces st.board.center Horizontal