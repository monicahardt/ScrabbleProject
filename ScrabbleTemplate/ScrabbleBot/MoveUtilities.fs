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

let nextDict (currentDict: Dict) (id: uint32) (pieces: Map<uint32, tile>) = step (getCharFromId pieces id) currentDict

let makeAWord (word: uint32 list) (pieces: Map<uint32, tile>): string = 
    List.fold(fun acc id -> 
                                            acc + (string (getCharFromId pieces id))) "" word

let getHandAsList (hand: MultiSet<uint32>) =
    toList hand     
    
let makeMove (result: uint32 list list)(pieces: Map<uint32, tile>) = 
    let wordToPlay = result.Head
    let coordsTest = [(0,0); (0,1); (0,2); (0,3)]

    let rec aux i acc = 
        if(i < wordToPlay.Length) then
            let letter = wordToPlay.[i]
            let coordToPlay = coordsTest.[i]
            let tileToPlay = Set.fold(fun _ (c,i) -> (c,i)) ('å',0) pieces.[letter]
            let thisPiece = (coordToPlay,(letter,tileToPlay))
            aux (i+1) (thisPiece :: acc)
        else acc
    aux 0 []