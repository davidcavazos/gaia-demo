module Data.Products exposing (..)

import Data.Locations exposing (Location(..))
import Resources exposing (Resources)


type alias Product =
    { name : String
    , from : Location
    , uses : Resources
    , requires : List ( String, Float )
    }


products : List Product
products =
    [ { name = "Endangered species chocolate (bar)", from = UnitedStates, uses = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }
    , { name = "KitKat (bar)", from = UnitedStates, uses = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 83.82 ), ( "Sugar (g)", 43.18 ) ] }
    , { name = "Cocoa (g)", from = UnitedStates, uses = { air = 0, water = 17, land = 0.02 }, requires = [] }
    , { name = "Sugar (g)", from = UnitedStates, uses = { air = 0, water = 17, land = 0.02 }, requires = [] }
    ]
