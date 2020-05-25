{-|
Module      : EKnets
Description : Short description
Copyright   : (c) Alice Rixte, 2020
License     : GPL-3
Maintainer  : alice.rixte@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


-- {-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
-- {-# LANGUAGE TypeOperators                #-}
-- {-# LANGUAGE ConstraintKinds              #-}
-- {-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE UnicodeSyntax                #-}
-- {-# LANGUAGE AllowAmbiguousTypes          #-}

-- {-# LANGUAGE CPP                          #-}
-- #if __GLASGOW_HASKELL__ >= 800
-- {-# LANGUAGE UndecidableSuperClasses      #-}
-- #endif

module EKnets
    ( ThinCat(..)
    , MonoidCat
    -- , SimpleCat
    , Iso
    )
where

import           Prelude                 hiding ( id
                                                , const
                                                , (.)
                                                )
import           Control.Category.Constrained
import           Data.Constraint                ( Constraint )
import qualified Data.Function                 as Func
                                                ( id
                                                , const
                                                , (.)
                                                )
-- import           Data.Monoid                    ( Monoid(..) )
-- import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Functor.Const             ( Const(..) )
import Control.Monad.Constrained




newtype OneMorphCat a b = OneMorphCat {onlyMorphism ::()}

instance Category OneMorphCat where
    id = OneMorphCat ()
    (.) = \(OneMorphCat ()) -> (\(OneMorphCat ()) -> OneMorphCat ())

type ThinCat = Kleisli Maybe OneMorphCat

type Cat a b  = ()



-- newtype ApplyMonadCat m k a b =
--     ApplyMonadCat {
--         liftMorphism :: (Category k, Monad m, Object k (m a), Object k (m b)) => k a b -> m (k (m a) (m b))}

-- monadId :: (Category k, Object k a,  Monad m) => ApplyMonadCat m k a a 
-- monadId = ApplyMonadCat { liftMorphism = \f -> return id} 

-- monadComp :: (Category k,  Monad m, 
--     Object k (m a), Object k (m b), Object k (m c))
--      => ApplyMonadCat m k (m b) (m c)
--      -> ApplyMonadCat m k (m a) (m b)
--      -> ApplyMonadCat m k (m a) (m c)
-- monadComp ApplyMonadCat { liftMorphism = retBC} 
--     ApplyMonadCat { liftMorphism = retAB} = 
--         ApplyMonadCat {liftMorphism = \f -> return f >>=  }

-- --         k b c -> k a b -> k a c 
-- --         m k (m b) (m c)
-- --         m k (m a) (m b)
-- --         a -> m a
-- --         m a -> (a -> m b) -> m b
-- --         m a -> m b -> m b

-- instance Category k => Category (ThinCat k) where
--     type Object (ThinCat k) o = Object k o
--     id  = thinId
--     (.) = thinCompose



class Category k => MonoidCategory x k where
    reflComp :: Object k x => k x x -> k x x -> k x x

newtype MonoidCat (x :: *) (k :: * -> * -> *) (a :: *) (b :: *)  =
     MonoidCat {
         getMonoid :: k a b -> ConstrainedCategory k ((~) x) a b
        }

instance Category (MonoidCat x k) where
    type Object (MonoidCat x k) o = (~) x o
    id  = id
    (.) = (.)

instance (Category k, x ~ a) => MonoidCategory (Const x a) (MonoidCat x k)  where
    reflComp = (.)


-- type SimpleCat x = MonoidCat x (ThinCat (->))



instance (Category k, x ~ a, x ~ b) => Semigroup ((MonoidCat x k) a b) where
    (<>) = (.)

instance (Category k, x ~ a, x ~ b) => Monoid ((MonoidCat x k) a b) where
    mempty = id





data Iso  k a b = Iso{
    isomorphism :: k a b,
    invert :: k b a
}

instance Category k => Category (Iso k) where
    id  = id
    (.) = (.)






