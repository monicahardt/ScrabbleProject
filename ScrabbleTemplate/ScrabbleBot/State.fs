
module internal State
open ScrabbleUtil
open MultiSet
open ScrabbleUtil.DebugPrint

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
        occupiedSquares : Map<coord, (uint32 * (char*int))> //mapping a coordinate to a tuple of (id * tile)
        possibleAnchors : Set<coord>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; 
                occupiedSquares = Map.empty<coord, uint32 * (char*int)>; possibleAnchors = Set.empty}
               
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let occupiedSquares st          = st.occupiedSquares
    let possibleAnchors st = st.possibleAnchors

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
        let updatedOccSpuares = List.fold(fun squares placedPiece -> Map.add (fst placedPiece) (snd placedPiece) squares) st.occupiedSquares ids // update used squares
        {st with occupiedSquares = updatedOccSpuares}

    let changeHand (newPieces: list<uint32 * uint32>) (st: state) = 
       let stateWithEmptyHand = {st with hand = empty}
       addNewTiles newPieces stateWithEmptyHand      


    //after a word has been placed we want to update good starting positions aka possible anchors to play at
    //an achor is a square adjecent to a word already placed on the board, which is needed every time we make a move*
    //* except for the first move where the board is empty
    let updatePossibleAnchors (placedTiles: list<coord * (uint32 * (char * int))>) (st: state) =
        let rec aux (tilesLeft: list<coord * (uint32 * (char * int))>) (acc: Set<coord>)  =
            match tilesLeft with
            |[] -> acc
            |tile:: tiles -> 
                let Tcoord = (fst tile)
                let rightAnchor = (((fst Tcoord) + 1), (snd Tcoord))
                let leftAnchor = (((fst Tcoord) - 1), (snd Tcoord))
                let upAnchor = (((fst Tcoord)), (snd Tcoord) - 1) //possible flipped these to y's
                let downAnchor = (((fst Tcoord)), (snd Tcoord) + 1)

                let anchorsToCheck = [rightAnchor; leftAnchor; upAnchor; downAnchor]  

                List.fold(fun accFold coord->
                    match Map.tryFind (coord) st.occupiedSquares with
                    |Some x -> accFold
                    |None -> 
                        Set.add coord accFold //if the anchor is not in occupiedSquares we add it to the set of coors that is our crossset
                ) acc anchorsToCheck |> aux tiles

        let updatedAnchors = aux placedTiles st.possibleAnchors
        {st with possibleAnchors = updatedAnchors}
