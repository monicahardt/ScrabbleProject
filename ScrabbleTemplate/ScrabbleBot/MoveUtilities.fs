module internal MoveUtilities

open ScrabbleUtil
open MultiSet
open Dictionary
open ScrabbleUtil.DebugPrint

 // ------------ HELPER FUNCTIONS ------------
type Direction =
|Vertical
|Horizontal
    
let getCharFromId (pieces: Map<uint32,tile>) (id: uint32) =
    let tile = Map.find id pieces
    (fst (List.head (Set.toList tile)))

let nextDict (currentDict: Dict) (id: uint32) (pieces: Map<uint32, tile>) = step (getCharFromId pieces id) currentDict

let makeAWord (word: uint32 list) (pieces: Map<uint32, tile>): string = 
    List.fold(fun acc id -> 
                                            acc + (string (getCharFromId pieces id))) "" word

let getHandAsList (hand: MultiSet<uint32>) =
    toList hand     


let makeMove (result: uint32 list)(pieces: Map<uint32, tile>) (coords: coord list) = 
    let wordToPlay = result
    debugPrint "\nMAKE MOVE BROKEN\n"
    let rec aux i acc = 
        debugPrint "\nMAKE MOVE UNIFINITY\n"
        if(i < wordToPlay.Length) then
            let letter = wordToPlay.[i]
            let coordToPlay = coords.[i]
            let tileToPlay = 
                if letter = 0u then ('A',0)
                else Set.fold(fun _ (c,i) -> (c,i)) ('å',0) pieces.[letter]
            let thisPiece = (coordToPlay,(letter,tileToPlay))
            aux (i+1) (thisPiece :: acc)
        else acc
    aux 0 []
