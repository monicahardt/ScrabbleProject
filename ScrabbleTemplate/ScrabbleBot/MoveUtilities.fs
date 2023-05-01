module internal MoveUtilities

open ScrabbleUtil
open State
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
    
let makeMove (result: uint32 list list)(pieces: Map<uint32, tile>) = 
    debugPrint (sprintf "MakeMove called with resultlist of size: %d" result.Length)

    let wordToPlay = List.fold(fun (acc: uint32 list) (lst: uint32 list) -> if (lst.Length > acc.Length) then lst else acc) [] result

    //let wordToPlay = result.Head
    let coordsTest = [(0,0); (0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (0,7); (0,8); (0,9); (0,10)]

    let rec aux i acc = 
        debugPrint "RUNNNING FOREVER"
        if(i < wordToPlay.Length) then
            let letter = wordToPlay.[i]
            let coordToPlay = coordsTest.[i]
            let tileToPlay = Set.fold(fun _ (c,i) -> (c,i)) ('å',0) pieces.[letter]
            let thisPiece = (coordToPlay,(letter,tileToPlay))
            aux (i+1) (thisPiece :: acc)
        else acc
    aux 0 []

let getNextCoordinate (pos: coord) (offset: int32) (dir: Direction) =
    match dir with
    | Vertical -> (fst pos), (snd pos + offset)
    | Horizontal -> ((fst pos) + offset, (snd pos))