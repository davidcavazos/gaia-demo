module Product exposing (..)

import Dict
import Footprint exposing (Footprint)


{-| Products table.

    get "Endangered species chocolate (bar)"
    --> Just { footprint = {air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }

-}
get : String -> Maybe { footprint : Footprint, requires : List ( String, Float ) }
get name =
    Dict.fromList
        [ ( "Endangered species chocolate (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }
          )
        , ( "KitKat (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 83.82 ), ( "Sugar (g)", 43.18 ) ] }
          )
        , ( "Cocoa (g)"
          , { footprint = { air = 0, water = 17, land = 0.02 }, requires = [] }
          )
        , ( "Sugar (g)"
          , { footprint = { air = 0, water = 17, land = 0.02 }, requires = [] }
          )
        ]
        |> Dict.get name


{-| Collect the footprint for a single unit of a product (recursive).

    totalFootprint "Cocoa (g)" --> Just { air = 0, water = 17, land = 0.02 }

    totalFootprint "Endangered species chocolate (bar)" --> Just { air = 0, water = 1496.0, land = 1.76 }

-}
totalFootprint : String -> Maybe Footprint
totalFootprint productName =
    Maybe.andThen
        (\{ footprint, requires } ->
            List.map
                (\( name, units ) -> Maybe.map (Footprint.map ((*) units)) (totalFootprint name))
                requires
                |> List.foldl
                    (Maybe.map2 (Footprint.map2 (+)))
                    (Just footprint)
        )
        (get productName)
