module Product exposing (..)

import Area exposing (Area(..))
import Dict exposing (Dict)
import Location exposing (Location(..))
import Resources exposing (Resources)
import Volume exposing (Volume(..))


type alias Product =
    { from : Location
    , uses : Resources
    , requires : Dict String Float
    }


{-| Products table.

    import Area exposing (Area(..))
    import Dict
    import Location exposing (Location(..))
    import Volume exposing (Volume(..))

    get "Endangered species chocolate (bar)"
    --> Just { from = UnitedStates, uses = { air = Liters 0, water = Liters 0, land = SquareMeters 0 }, requires = Dict.fromList [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }

-}
get : String -> Maybe Product
get name =
    Dict.fromList
        [ ( "Endangered species chocolate (bar)"
          , { from = UnitedStates, uses = { air = Liters 0, water = Liters 0, land = SquareMeters 0 }, requires = Dict.fromList [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }
          )
        , ( "KitKat (bar)"
          , { from = UnitedStates, uses = { air = Liters 0, water = Liters 0, land = SquareMeters 0 }, requires = Dict.fromList [ ( "Cocoa (g)", 83.82 ), ( "Sugar (g)", 43.18 ) ] }
          )
        , ( "Cocoa (g)"
          , { from = UnitedStates, uses = { air = Liters 0, water = Liters 17, land = SquareMeters 0.02 }, requires = Dict.empty }
          )
        , ( "Sugar (g)"
          , { from = UnitedStates, uses = { air = Liters 0, water = Liters 17, land = SquareMeters 0.02 }, requires = Dict.empty }
          )
        ]
        |> Dict.get name


{-| Collect the footprint for a single unit of a product (recursive).

    import Area exposing (Area(..))
    import Volume exposing (Volume(..))

    totalResources "Cocoa (g)" --> Just { air = Liters 0, water = Liters 17, land = SquareMeters 0.02 }
    totalResources "Endangered species chocolate (bar)" --> Just { air = Liters 0, water = Liters 1496.0, land = SquareMeters 1.76 }

-}
totalResources : String -> Maybe Resources
totalResources productName =
    Maybe.andThen
        (\{ uses, requires } ->
            Dict.map
                (\name units -> Maybe.map (Resources.map ((*) units)) (totalResources name))
                requires
                |> Dict.values
                |> List.foldl
                    (Maybe.map2 (Resources.map2 (+)))
                    (Just uses)
        )
        (get productName)
