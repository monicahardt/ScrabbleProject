module internal MoveUtilities

open ScrabbleUtil
open State
open MultiSet
open Dictionary
open ScrabbleUtil.DebugPrint

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

let nextDict (currentDict: Dict) (id: uint32) (pieces: Map<uint32, tile>) = step (getCharFromId pieces id) currentDict

let makeAWord (word: uint32 list) (pieces: Map<uint32, tile>): string = 
    List.fold(fun acc id -> 
                                            acc + (string (getCharFromId pieces id))) "" word

    //looping the entire hand with a function
let getHandAsList (hand: MultiSet<uint32>) =
    toList hand 


let createFinalInputString (coords: (int * int) list) (tiles: string list) =
    let rec addCoordAndTile (coord: (int * int) list) (tile: string list) (acc: string)=
        match (coord, tile) with
        | (c::cs, t::ts) -> 
            let newString = "(" + (string (fst c)) + " " + (string (snd c)) + " " + t + ") " 
            let newAcc = acc + newString
            addCoordAndTile cs ts newAcc
        | _ -> acc
    addCoordAndTile coords tiles ""


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
    