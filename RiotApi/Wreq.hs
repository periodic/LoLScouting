{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module RiotApi.Wreq where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.RateLimit
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Map (Map, elems)
import Data.Text (Text, unpack)
import Data.Time.Units
import GHC.Generics (Generic)
import Network.Wreq

import RiotApi.Types

riotBaseApiUrl :: String
riotBaseApiUrl = "https://na.api.pvp.net/api/lol/na"

riotBaseStaticUrl :: String
riotBaseStaticUrl = "https://na.api.pvp.net/api/lol/static-data/na"

type ApiRequestFunc = (Options, String) -> IO (Response ByteString)

data RiotApiState = RiotApiState
  { _apiKey :: Text
  , _getApiFunc :: ApiRequestFunc
  }
makeClassy ''RiotApiState

newtype RiotApi a = RiotApi {
  runRiotApiInternal :: StateT RiotApiState IO a
} deriving (Functor, Applicative, Monad, MonadState RiotApiState, MonadIO, MonadThrow)

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn

runRiotApi :: Text -> RiotApi a -> IO a
runRiotApi key m = do
  f <- rateLimitExecution (1 :: Second) (uncurry getWith)
  let st = RiotApiState key f
  flip evalStateT st . runRiotApiInternal $ m

getApi :: Options -> String -> RiotApi (Response ByteString)
getApi opts url = do
  getFunc <- use getApiFunc
  key <- use apiKey
  logMsg $ "requesting: " ++ url
  liftIO $ getFunc (opts & param "api_key" .~ [key], url)


getRequestUrl :: RiotReq a -> String
getRequestUrl (GetSummonerByName names) = 
  let path = "/v1.4/summoner/by-name/"
      summonerNames = intercalate "," $ names
  in concat [riotBaseApiUrl, path, summonerNames]
getRequestUrl (GetTeamBySummoner ids)   =
  let path = "/v2.4/team/by-summoner/"
      summonerNames = intercalate "," . map show $ ids
  in concat [riotBaseApiUrl, path, summonerNames]
getRequestUrl (GetMatchDetail    id)    =
  let path = "/v2.2/match/"
  in concat [riotBaseApiUrl, path, show id]
getRequestUrl (GetChampions)            =
  let path = "/v1.2/champion"
  in concat [riotBaseStaticUrl, path]

getResponseParser :: MonadThrow m => RiotReq a -> Response ByteString -> m a
getResponseParser (GetSummonerByName names) resp = (asJSON resp :: MonadThrow m => m (Response (Map String Summoner))) >>= (\json -> return $ json ^. responseBody . to elems)
getResponseParser (GetTeamBySummoner ids)   resp = asJSON resp >>= (\json -> return $ json ^. responseBody)
getResponseParser (GetMatchDetail    id)    resp = asJSON resp >>= (\json -> return $ json ^. responseBody)
getResponseParser (GetChampions)            resp = asJSON resp >>= (\json -> return $ json ^. responseBody)

getRequest :: RiotReq a -> RiotApi a
getRequest req =
  let url = getRequestUrl req
      parser = getResponseParser req
  in getApi defaults url >>= parser

--------------------------------------------------------------------------------
--  GetSummonerByName
--------------------------------------------------------------------------------

getSummonerByName :: [UnvalidatedSummonerName] -> RiotApi [Summoner]
getSummonerByName = getRequest . GetSummonerByName

--------------------------------------------------------------------------------
--  GetTeamBySummoner
--------------------------------------------------------------------------------

getSummonerTeams :: [SummonerId] -> RiotApi (Map Text [Team])
getSummonerTeams = getRequest . GetTeamBySummoner

--------------------------------------------------------------------------------
--  GetMatchDetail
--------------------------------------------------------------------------------

getMatchDetail :: MatchId -> RiotApi MatchDetail
getMatchDetail = getRequest . GetMatchDetail

--------------------------------------------------------------------------------
--  GetChampions
--------------------------------------------------------------------------------

getChampions :: RiotApi ChampionList
getChampions = getRequest GetChampions
