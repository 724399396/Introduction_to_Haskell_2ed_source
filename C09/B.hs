{-# LANGUAGE StandaloneDeriving #-}
module B where

import A

deriving instance (Show a) => Show (Foo a)
