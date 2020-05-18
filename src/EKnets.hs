-- {-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
-- {-# LANGUAGE TypeOperators                #-}
-- {-# LANGUAGE ConstraintKinds              #-}
-- {-# LANGUAGE FlexibleContexts             #-}
-- {-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE UnicodeSyntax                #-}
-- {-# LANGUAGE AllowAmbiguousTypes          #-}

-- {-# LANGUAGE CPP                          #-}
-- #if __GLASGOW_HASKELL__ >= 800
-- {-# LANGUAGE UndecidableSuperClasses      #-}
-- #endif

module EKnets
    ( 

    )
where

import Control.Category.Constrained
import Data.Constraint (Constraint)
import qualified Data.Function as Func (id,const,(.))

newtype ThinCat a b = ThinCat {arrow :: Maybe (a -> b)}
thinId :: ThinCat a a
thinId = ThinCat {arrow = Just Func.id}
thinCompose :: ThinCat b c -> ThinCat a b -> ThinCat a c
thinCompose ThinCat {arrow = Just f} ThinCat {arrow = Just g} = 
    ThinCat {arrow = Just $ f Func.. g}

instance Category ThinCat where
    id = thinId
    (.) = thinCompose

type MonoidCat k x = ConstrainedCategory k ((~) x)
type SimpleCat x = MonoidCat ThinCat x 









   





