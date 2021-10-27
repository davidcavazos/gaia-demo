module Product exposing (..)

import Data.Products exposing (Product, products)
import Dict exposing (Dict)
import Resources exposing (Resources)


productsDict : Dict String Product
productsDict =
    products
        |> List.map (\p -> ( p.name, p ))
        |> Dict.fromList


{-| Get a product from a product name.

    import Data.Locations exposing (Location(..))

    get "Endangered species chocolate (bar)"
    --> Just { name = "Endangered species chocolate (bar)", from = UnitedStates, uses = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }

-}
get : String -> Maybe Product
get name =
    Dict.get name productsDict


{-| Collect the footprint for a single unit of a product (recursive).

    totalResources "Cocoa (g)" --> Just { air = 0, water = 17, land = 0.02 }

    totalResources "Endangered species chocolate (bar)" --> Just { air = 0, water = 1496.0, land = 1.76 }

-}
totalResources : String -> Maybe Resources
totalResources productName =
    Maybe.andThen
        (\{ uses, requires } ->
            requires
                |> List.map (\( name, amount ) -> Maybe.map (Resources.map ((*) amount)) (totalResources name))
                |> List.foldl
                    (Maybe.map2 (Resources.map2 (+)))
                    (Just uses)
        )
        (get productName)
