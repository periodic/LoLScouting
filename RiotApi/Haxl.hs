{-# LANGUAGE DataKinds, TypeFamilies, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module RiotApi.Haxl where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Data.Map (Map)
import Data.Text
import Data.Typeable
import Haxl.Core
import Network.HTTP.Client

import RiotApi.Types
import RiotApi.Wreq hiding (getSummonerByName, getTeamMatches, GetTeamBySummoner, getChampions)

type RiotApi = GenHaxl ()

instance Show1 RiotReq where
  show1 = show

instance StateKey RiotReq where
  data State RiotReq = RiotState
      { apiKey :: RiotApiKey
      , manager :: Manager
      , numThreads :: Int
      }

initGlobalState :: Int -> RiotApiKey -> IO (State RiotReq)
initGlobalState threads key = do
  manager <- newManager defaultManagerSettings
  return RiotState
    { apiKey = key
    , manager = manager
    , numThreads = threads
    }

instance DataSourceName RiotReq where
  dataSourceName _ = "Riot"

instance DataSource u RiotReq where
  fetch = riotFetch

riotFetch :: State RiotReq -> Flags -> u -> [BlockedFetch RiotReq] -> PerformFetch
riotFetch RiotState{..} _flags _ bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync apiKey manager sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: RiotApiKey -> Manager -> QSem -> BlockedFetch RiotReq -> IO (Async ())
fetchAsync apiKey httpManager sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
      e <- Control.Exception.try $ runRiotApi apiKey . getRequest $ req
      case e of
        Left ex -> putFailure rvar (ex :: SomeException)
        Right a -> putSuccess rvar a

--------------------------------------------------------------------------------
--  GetSummonerByName
--------------------------------------------------------------------------------

getSummonerByName :: [UnvalidatedSummonerName] -> GenHaxl u [Summoner]
getSummonerByName = dataFetch . GetSummonerByName

--------------------------------------------------------------------------------
--  GetTeamBySummoner
--------------------------------------------------------------------------------

getSummonerTeams :: [SummonerId] -> GenHaxl u (Map Text [Team])
getSummonerTeams = dataFetch . GetTeamBySummoner

--------------------------------------------------------------------------------
--  GetMatchDetail
--------------------------------------------------------------------------------

getMatchDetail :: MatchId -> GenHaxl u MatchDetail
getMatchDetail = dataFetch . GetMatchDetail

--------------------------------------------------------------------------------
--  GetChampions
--------------------------------------------------------------------------------

getChampions :: GenHaxl u ChampionList
getChampions = dataFetch GetChampions
