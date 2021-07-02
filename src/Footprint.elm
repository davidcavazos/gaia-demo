module Footprint exposing (..)


type alias Footprint =
    { air : Float --   cubic meters per unit
    , water : Float -- liters per unit
    , land : Float --  square meters per unit
    }


map : (Float -> Float) -> Footprint -> Footprint
map f fp =
    { air = f fp.air
    , water = f fp.water
    , land = f fp.land
    }


map2 : (Float -> Float -> Float) -> Footprint -> Footprint -> Footprint
map2 f fp1 fp2 =
    { air = f fp1.air fp2.air
    , water = f fp1.water fp2.water
    , land = f fp1.land fp2.land
    }
