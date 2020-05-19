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
    ()
where

import           Prelude                        hiding (id, const, (.))
import           Control.Category.Constrained   as ConstrCategories
import           Data.Constraint                ( Constraint )
import qualified Data.Function                 as Func
                                                ( id
                                                , const
                                                , (.)
                                                )
import qualified Data.Monoid                    (Monoid(..) )
import Data.Functor.Const                       (Const(..))

newtype ThinCat a b = ThinCat {arrow :: Maybe (a -> b)}
thinId :: ThinCat a a
thinId = ThinCat { arrow = Just Func.id }
thinCompose :: ThinCat b c -> ThinCat a b -> ThinCat a c
thinCompose ThinCat { arrow = Just f } ThinCat { arrow = Just g } =
    ThinCat { arrow = Just $ f Func.. g }

instance Category ThinCat where
    id  = thinId
    (.) = thinCompose



class Category k => MonoidCategory x k where
    reflComp :: Object k x => k x x -> k x x -> k x x

newtype MonoidCat (x :: *) (k :: * -> * -> *) (a :: *) (b :: *)  =
     MonoidCat {
         getMonoid :: k a b -> ConstrainedCategory k ((~) x) a b
        }

instance Category (MonoidCat x k) where
    type Object (MonoidCat x k) o = (~) x o 
    id = ConstrCategories.id
    (.) = (ConstrCategories..)

instance (Category k, x ~ a) => MonoidCategory (Const x a) (MonoidCat x k)  where
    reflComp = (.)


type SimpleCat x = MonoidCat x ThinCat 















