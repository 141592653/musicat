{-# LANGUAGE GADTs #-}

module EKnets
    ( 

    )
where

import Control.Category
import qualified Data.Function as Func (id,(.))


newtype ThinCat a b = ThinCat {arrow :: a -> b}
thinId :: ThinCat a a
thinId = ThinCat {arrow = Func.id}
thinCompose :: ThinCat b c -> ThinCat a b -> ThinCat a c
thinCompose ThinCat {arrow = f} ThinCat {arrow = g} = 
    ThinCat {arrow = f Func.. g}

data OneObjCat a b where
    EndoMorph :: a ~ b => (a -> b)-> OneObjCat a b

oneObjId :: OneObjCat a a
oneObjId = EndoMorph Func.id 
oneObjCompose :: OneObjCat b c -> OneObjCat a b -> OneObjCat a c
oneObjCompose (EndoMorph f) (EndoMorph g) = EndoMorph (f Func.. g)

instance Category ThinCat where
    id = thinId
    (.) = thinCompose

instance Category OneObjCat where 
    id = oneObjId
    (.) = oneObjCompose