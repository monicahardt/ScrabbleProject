
module internal State
open ScrabbleUtil
open MultiSet

    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        //occupiedSquares : Map<coord, uint32 * (char*int)> //mapping a coordinate to a tuple of (id * tile)
        occupiedSquares : Map<coord, uint32> //mapping a coordinate to a tuple of (id * tile)
        nextTile : ((uint32 * coord) * bool)
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; 
                //occupiedSquares = Map.empty<coord, uint32 * (char*int)>; nextTile = ((100u, (0,0)), true)}
                occupiedSquares = Map.empty; nextTile = ((100u, (0,0)), true)}
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let occupiedSquares st          = st.occupiedSquares
    let nextTile st = st.nextTile


    let removeTileFromHand (hand: MultiSet<uint32>) (id: uint32) =
        MultiSet.removeSingle id hand

    let removeTilesFromHand (st: state) (ids: list<coord * (uint32 * (char * int))>) =
        let updatedHand = List.fold(fun acc tile -> removeTileFromHand acc (fst(snd tile))) st.hand ids
        {st with hand = updatedHand}

    let addTileToHand (hand: MultiSet<uint32>) (id: uint32) (numOf: uint32) =
       add id numOf hand

    let addNewTiles (newTiles: list<uint32 * uint32> ) (st: state)  =
        let updatedHand = List.fold(fun acc tile -> addTileToHand acc (fst tile) (snd tile)) st.hand newTiles   
        {st with hand = updatedHand}

       
    let updateOccSquares (ids: list<coord * (uint32 * (char * int))>) (st: state) =
        let updatedOccSpuares = List.fold(fun squares (coord,(id,(_,_))) -> Map.add coord id squares) st.occupiedSquares ids // update used squares
        {st with occupiedSquares = updatedOccSpuares}

    let changeHand (newPieces: list<uint32 * uint32>) (st: state) = 
       let stateWithEmptyHand = {st with hand = empty}
       addNewTiles newPieces stateWithEmptyHand      
