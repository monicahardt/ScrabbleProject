module internal Tile


type Tile =
    | Normal of (uint32 * (char * int))
    | Blank of (uint32 * Map<char, int>)

//checking if the tile we try to put on the board is in the crossset of the tile we are looking at
let isTileAllowed (allowedLetters: Set<char>) (tile: Tile) = 
    match tile with
    |Normal(id,(ch,point)) -> Set.contains ch allowedLetters
    |Blank _  -> if allowedLetters.Count = 0 then false else true

let makeTile (id: uint32) t = 
    match id with
    |0u -> Blank(id, Map.ofSeq (t))
    |_-> Normal(id, Seq.head t)