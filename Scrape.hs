{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Maybe as Maybe
import Data.Monoid
import Data.Set as S
import Data.Text as T
import HFlags
import System.Environment (getArgs)
import Haxl.Core

import RiotApi.Types
import RiotApi.Haxl
import Parsers

-- "e0ae40cd-dcf2-4cfd-9530-6eb037e54648"
defineFlag "key" ("" :: Text) "Your Riot API developer key"

