module Model exposing (..)


type Tile
    = CoveredBomb
    | CoveredClear
    | MarkedBomb
    | MarkedClear
    | ExposedClear
      -- only shown when the game is lost
    | ExposedBomb
    | Detonated


type alias TileList =
    List ( Tile, Int )


type alias Model =
    { square : Int
    , tiles : TileList
    }


type Msg
    = NewGame
    | NewBoard (List Tile)
    | DoClear Int
    | DoMark Int
