{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Network (PortID (PortNumber))
import           Language.Haskell.TH.Syntax

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
    in share [mkPersist mongoSettings] [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Show
BlogPost
  title String
  authorId PersonId
  deriving Show
|]

runDBActions actions =
  withMongoDBConn "test" "localhost" (PortNumber 27017) Nothing 2000 $ \pool ->
    runMongoDBPool master actions pool

actions = do
  johnId <- insert $ Person "John Doe" $ Just 35
  janeId <- insert $ Person "Jane Doe" Nothing

  insert $ BlogPost "My fr1st p0st" johnId
  insert $ BlogPost "One more for good measure" johnId

  oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
  liftIO $ print (oneJohnPost :: [Entity BlogPost])

  john <- get johnId
  liftIO $ print (john :: Maybe Person)

  delete janeId
  deleteWhere [BlogPostAuthorId ==. johnId]

main :: IO ()
main = do
  runDBActions actions
