
module internal State
open ScrabbleUtil
open MultiSet


    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type Direction =
    |Horizontal
    |Vertical

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        numPlayers : uint32
        playerNumber  : uint32
        playerTurn :uint32
        hand          : MultiSet.MultiSet<uint32>
        occupiedSquares : Map<coord, (uint32 * (char*int))> //mapping a coordinate to a tuple of (id * tile)
        setOfPlayers : Set<uint32>
    }

    let mkState b d np pn pt h = {board = b; dict = d; numPlayers = np;  playerNumber = pn; playerTurn = pt; hand = h; 
                occupiedSquares = Map.empty<coord, uint32 * (char*int)>; setOfPlayers = Set.empty<uint32>}
               
    let board st            = st.board
    let dict st             = st.dict
    let numPlayers st = st.numPlayers
    let playerNumber st     = st.playerNumber
    let playerTurn st = st.playerTurn
    let hand st             = st.hand
    let occupiedSquares st  = st.occupiedSquares
    let setOfPlayers st = st.setOfPlayers

    let removeTileFromHand (hand: MultiSet<uint32>) (id: uint32) = 
        removeSingle id hand

    let removeTilesFromHand (st: state) (ids: list<coord * (uint32 * (char * int))>) =
        let updatedHand = List.fold(fun acc tile -> removeTileFromHand acc (fst(snd tile))) st.hand ids
        {st with hand = updatedHand}

    let addTileToHand (hand: MultiSet<uint32>) (id: uint32) (numOf: uint32) = 
        add id numOf hand

    let addNewTiles (newTiles: list<uint32 * uint32> ) (st: state)  =
        let updatedHand = List.fold(fun acc tile -> addTileToHand acc (fst tile) (snd tile)) st.hand newTiles   
        {st with hand = updatedHand}
       
    let updateOccSquares (ids: list<coord * (uint32 * (char * int))>) (st: state) =
        let updatedOccSpuares = List.fold(fun squares placedPiece -> Map.add (fst placedPiece) (snd placedPiece) squares) st.occupiedSquares ids // update used squares
        {st with occupiedSquares = updatedOccSpuares}

    let changeHand (newPieces: list<uint32 * uint32>) (st: state) = 
       let stateWithEmptyHand = {st with hand = empty}
       addNewTiles newPieces stateWithEmptyHand

    let addPlayers (id: uint32) (set: Set<uint32>) (st: state) =
        let updatedSetOfPlayer = Set.add id set
        {st with setOfPlayers = updatedSetOfPlayer}

    let removePlayers (id: uint32) (set: Set<uint32>) (st: state) =
        let updatedSetOfPlayer = Set.remove id set
        let updatedNumPlayers = st.numPlayers - 1u
        {st with setOfPlayers = updatedSetOfPlayer; numPlayers = updatedNumPlayers}