{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.PEM.Writer
License     : BSD-style
Copyright   : (c) 2010-2018 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : portable
-}

module Data.PEM.Writer
  ( pemWriteBS
  , pemWriteLBS
  ) where

import           Data.Base64.Types ( extractBase64 )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import           Data.ByteString.Base64 ( encodeBase64' )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import           Data.PEM.Types ( PEM (..) )

-- | Write a t'PEM' to a lazy 'Data.ByteString.Lazy.ByteString'.
pemWrite :: PEM -> L.ByteString
pemWrite pem = L.fromChunks ([begin, header] ++ section ++ [end])
 where
  begin   = B.concat ["-----BEGIN ", sectionName, "-----\n"]
  end     = B.concat ["-----END ", sectionName, "-----\n" ]
  section :: [ByteString]
  section = map encodeLine $ splitChunks $ pemContent pem
  header :: ByteString
  header  = if null $ pemHeader pem
              then B.empty
              else B.concat (concatMap toHeader (pemHeader pem) ++ ["\n"])
  toHeader :: (String, ByteString) -> [ByteString]
  toHeader (k,v) = [ BC.pack k, ":", v, "\n" ]
  -- expect only ASCII. need to find a type to represent it.
  sectionName = BC.pack $ pemName pem
  encodeLine l = (extractBase64 . encodeBase64') l `B.append` "\n"

  splitChunks b
    | B.length b > 48 = let (x,y) = B.splitAt 48 b in x : splitChunks y
    | otherwise       = [b]

-- | Convert the specified t'PEM' to a strict 'ByteString'.
pemWriteBS :: PEM -> ByteString
pemWriteBS = B.concat . L.toChunks . pemWrite

-- | Convert the specified t'PEM' to a lazy 'Data.ByteString.Lazy.ByteString'.
pemWriteLBS :: PEM -> L.ByteString
pemWriteLBS = pemWrite
