module internal Tile


type Tile =
    | Normal of (uint32 * (char * int))
    | Blank of (uint32 * Map<char, int>)

let makeTile (id: uint32) t = 
    match id with
    |0u -> Blank(id, Map.ofSeq (t))
    |_-> Normal(id, Seq.head t)