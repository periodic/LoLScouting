{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.GameStats where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Network (PortID (PortNumber))
import           Language.Haskell.TH.Syntax
import           Data.Monoid
import           Data.Text

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
    in share [mkPersist mongoSettings] [persistLowerCase|
GameRecord
  championId Integer
  playerId Integer
  queueType Text
  duration Integer
  kills Integer
  deaths Integer
  assists Integer
  gamesTotal Integer
  gamesWon Integer
  creepScore Integer
  goldEarned Integer
  towersKilled Integer
  dragonsKilled Integer
  baronsKilled Integer
  greenWardsPurchased Integer
  pinkWardsPurchased Integer
  wardsPlaced Integer
  deriving Show
|]
