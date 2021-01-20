{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- 
Hopefully this will work now I have put LANGUAGE in upper case.

Do this inteactively by

:set -XFlexibleInstances -XFunctionalDependencies -XMultiParamTypeClasses

-}

import Data.Char
import Data.List
import System.IO

import Data.Monoid


class Category obj mor | mor -> obj where
  dom :: mor -> obj
  cod :: mor -> obj
  idy :: obj -> mor
  cmp :: mor -> mor -> Maybe mor
  
-- instance Monoid mor => (Category () mor) where
    -- dom _ = ()
    -- cod _ = ()
    -- idy _ = mempty
    -- cmp m n = Just (m <> n)
    
data TwoObj = One | Two 
  deriving ( Eq , Ord, Show )

data TwoMor = TM (One, One) | TM (One,Two) | TM (Two,Two)

instance (Category TwoObj TwoMor)  where
    dom (d,c) = d
    cod (d,c) = c
    idy t = (t,t)
    cmp (b',c) (a,b) | b' == b      = Just (a,c)
                     | otherwise    = Nothing
