{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
Module      : Data.PEM.Types
License     : BSD-style
Copyright   : (c) 2010-2018 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : portable
-}

module Data.PEM.Types
  ( PEM (..)
  ) where

import           Basement.NormalForm ( NormalForm (..) )
import           Control.DeepSeq ( NFData (..) )
import           Data.ByteString ( ByteString )
import           GHC.Generics ( Generic )

-- | A type representing single PEM sections.
data PEM = PEM
  { pemName    :: String
    -- ^ The name of the section, found after the dash BEGIN tag.
  , pemHeader  :: [(String, ByteString)]
    -- ^ Optional key-value pairs header. The library does not currently
    -- serialize headers.
  , pemContent :: ByteString
    -- ^ Binary content of the section.
  }
  deriving (Eq, Generic, NFData, Show)

-- | The t'PEM' instance of 'NormalForm' is deprecated and will be removed from
-- a future version of this package. See the 'NFData' type class exported by
-- "Control.DeepSeq" of the @deepseq@ GHC boot package.
instance NormalForm PEM where
  toNormalForm pem =
    toNormalForm (pemName pem) `seq` nfLbs (pemHeader pem) `seq` pemContent pem `seq` ()
   where
    nfLbs []         = ()
    nfLbs ((s,bs):l) = toNormalForm s `seq` bs `seq` nfLbs l
