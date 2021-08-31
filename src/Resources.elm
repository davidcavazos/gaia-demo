module Resources exposing (..)

import Area exposing (Area(..))
import Currency exposing (Currency(..))
import Volume exposing (Volume(..))


type alias Resources =
    { air : Volume
    , water : Volume
    , land : Area
    }


{-| Calculate the resources footprint cost.

    import Area exposing (Area(..))
    import Currency exposing (Currency(..))
    import Volume exposing (Volume(..))

    cost { air = Liters 0, water = Liters 1496.0, land = SquareMeters 1.76 } --> USD 0.6972379617770906

-}
cost : Resources -> Currency
cost { air, water, land } =
    Currency.map3 (\a b c -> a + b + c)
        (airCost air)
        (waterCost water)
        (landCost land)


{-| Air resource cost.
-}
airCost : Volume -> Currency
airCost _ =
    USD 0


{-| Water resource cost.

    import Currency exposing (Currency(..))
    import Volume exposing (Volume(..))

    waterCost (Liters 1) --> USD 0.00040211373832598743

-}
waterCost : Volume -> Currency
waterCost (Liters liters) =
    let
        dollarsPerLiter =
            -- dollarsPerAcreFoot / litersPerAcreFoot
            496 / 1233481.85532
    in
    USD (liters * dollarsPerLiter)


{-| Land resource cost.

    import Area exposing (Area(..))
    import Currency exposing (Currency(..))

    landCost (SquareMeters 1) --> USD 0.05436125525080306

-}
landCost : Area -> Currency
landCost (SquareMeters squareMeters) =
    let
        dollarsPerSquareMeter =
            -- dollarsPerAcre / squareMetersPerAcre
            220 / 4047
    in
    USD (squareMeters * dollarsPerSquareMeter)


map : (Float -> Float) -> Resources -> Resources
map f a =
    { air = Volume.map f a.air
    , water = Volume.map f a.water
    , land = Area.map f a.land
    }


map2 : (Float -> Float -> Float) -> Resources -> Resources -> Resources
map2 f a b =
    { air = Volume.map2 f a.air b.air
    , water = Volume.map2 f a.water b.water
    , land = Area.map2 f a.land b.land
    }
