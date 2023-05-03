module internal Hand
open ScrabbleUtil
open State
open Tile


let getHand (st: State.state) (pieces: Map<uint32, tile>) : Tile list =
    let findATile (tileId: uint32) (pieces: Map<uint32, tile>) = Map.find tileId pieces
    let createTile id t = makeTile id t

    MultiSet.fold (fun (hand: Tile list) id _ -> 
        let foundT = findATile id pieces
        let madeTile = createTile id (Set.toSeq foundT)
        madeTile :: hand
     ) [] st.hand

let getCrossSet (dict: Dictionary.Dict) : Set<char> =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Seq.toList

    let rec aux (dict: Dictionary.Dict) (letters: char list) (allowed: Set<char>) =
        match letters with
        | [] -> allowed
        | c :: cs ->
            //is there exists a step towards the letter we add it, if not we don't
            //do we need to check here if a word ends?
            match Dictionary.step c dict with
            | Some _ -> aux dict cs (Set.add c allowed)
            | None -> aux dict cs allowed
    aux dict alphabet Set.empty