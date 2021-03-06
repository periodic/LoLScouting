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

-- "e0ae40cd-dcf2-4cfd-9530-6eb037e54648"
defineFlag "key" ("" :: Text) "Your Riot API developer key"

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = L.sortBy (\a b -> f a `compare` f b)

data ChampionStat = ChampionStat
  { wins :: Int
  , losses :: Int
  , kills :: Int
  , deaths :: Int
  , assists :: Int
  , gold :: Int
  , duration :: Int
  } deriving (Show)
instance Monoid ChampionStat where
  mempty = ChampionStat 0 0 0 0 0 0 0
  mappend (ChampionStat w1 l1 k1 d1 a1 g1 t1) (ChampionStat w2 l2 k2 d2 a2 g2 t2) =
    ChampionStat (w1 + w2) (l1 + l2) (k1 + k2) (d1 + d2) (a1 + a2) (g1 + g2) (t1 + t2)

newtype ChampionStats = CS {
  championStatsAsMap :: Map ChampionId ChampionStat
  } deriving (Show)
instance Monoid ChampionStats where
  mempty = CS mempty
  mappend (CS map1) (CS map2) = CS $ M.unionWith mappend map1 map2

newtype SummonerStats = SS {
  summonerStatsAsMap :: Map SummonerId ChampionStats
  } deriving (Show)
instance Monoid SummonerStats where
  mempty = SS mempty
  mappend (SS map1) (SS map2) = SS $ M.unionWith mappend map1 map2

data MatchSummary = MatchSummary
  { victory :: Bool
  , participants :: [(SummonerName, ChampionId)]
  } deriving (Show)

showMatchSummary :: Map SummonerId Summoner -> Map ChampionId Champion -> MatchSummary -> Text
showMatchSummary summonerMap championMap (MatchSummary victory participants) =
  let resolvedParticipants = L.map resolveNames participants
      victoryText = if victory then "Victory" else "Defeat"
      participantText = T.intercalate "\n" . Maybe.mapMaybe (fmap (\(s, c) -> T.concat [s, ": ", c]))
                                           $ resolvedParticipants
  in T.concat [victoryText, "\n", participantText]
  where
    -- TODO: This is probably a one-liner.
    resolveNames (summonerName, championId) =
      let mChampion = M.lookup championId championMap
      in case mChampion of
        Just champion -> Just (summonerName, champion ^. c_name)
        _             -> Nothing

showSummonerStats :: Map SummonerId Summoner -> Map ChampionId Champion -> SummonerStats -> Text
showSummonerStats summonerMap championMap (SS summonerStatMap) =
  let summonerStatList = M.toList summonerStatMap
  in T.intercalate "\n" . Maybe.mapMaybe (uncurry showIndividualStats) $ summonerStatList
  where
    showIndividualStats :: SummonerId -> ChampionStats -> Maybe Text
    showIndividualStats summonerId championStats = do
      summoner <- M.lookup summonerId summonerMap
      let name = summoner ^. s_name
      championStats <- showChampStats championStats
      return $ T.concat [name, "\n", championStats]
    showChampStats :: ChampionStats -> Maybe Text
    showChampStats (CS championStatsMap) = Just .
      T.intercalate "\n" . Maybe.mapMaybe (uncurry showIndividualChampionStat) . M.toList $ championStatsMap
    showIndividualChampionStat :: ChampionId -> ChampionStat -> Maybe Text
    showIndividualChampionStat championId championStat = do
      champion <- M.lookup championId championMap
      let name = champion ^. c_name
          champWins = wins championStat
          champLosses = losses championStat
          champGames = champWins + champLosses
          champWinPct = fromIntegral champWins / fromIntegral champGames * 100
          champKDA = fromIntegral (kills championStat + assists championStat) /
                     fromIntegral (deaths championStat)
      return $ T.concat [name, " ", T.pack $ show champGames, " ",
                         T.pack $ show champWinPct, "% ",
                         T.pack $ show champKDA, "KDA"]

summarizeMatch :: [SummonerId] -> MatchDetail -> (MatchSummary, SummonerStats)
summarizeMatch interestingSummoners match =
  -- TODO: Clean up these lenses.
  -- * Add dates
  -- * Add per-summoner-champion stats
  let
      -- set of interesting summonerIds
      interestingSummonerSet = S.fromList interestingSummoners

      -- get only the interesting participantIdentities
      interestingPIs =
        L.filter (\ident -> S.member (ident ^. pi_player . p_summonerId) interestingSummonerSet)
                 (match ^. md_participantIdentities)

      -- the interesting participantIds
      participantIds = interestingPIs ^.. traverse . pi_participantId
      participantSet = S.fromList participantIds

      -- summoner Ids in participant order
      unsortedSummonerIds = interestingPIs ^.. traverse . pi_player . p_summonerId
      summonerIds         = L.map snd . L.sort $ L.zip participantIds unsortedSummonerIds

      -- get only the ineresting Participants
      interestingParticipants =
          sortWith (view p_participantId)
          . L.filter (flip S.member participantSet . view p_participantId)
          $ match ^. md_participants

      -- get data from participant stats.
      participantIdsFromStats = interestingParticipants ^.. traverse . p_participantId
      championIds             = interestingParticipants ^.. traverse . p_championId
      participantStats        = interestingParticipants ^.. traverse . p_stats

      -- build ChampionStats
      kills   = ZipList $ participantStats ^.. traverse . ps_kills
      deaths  = ZipList $ participantStats ^.. traverse . ps_deaths
      assists = ZipList $ participantStats ^.. traverse . ps_assists
      gold    = ZipList $ participantStats ^.. traverse . ps_goldEarned
      victor  = ZipList $ participantStats ^.. traverse . ps_winner
      wins    = fmap (\b -> if b then 1 else 0) victor
      losses  = fmap (1-) wins
      durations = ZipList . L.repeat $ match ^. md_matchDuration
      championStatList = ChampionStat <$> wins
                                      <*> losses
                                      <*> kills
                                      <*> deaths
                                      <*> assists
                                      <*> gold
                                      <*> durations
      champIdsAndStats = L.zip championIds . getZipList $ championStatList
      championStats = L.map (CS . uncurry M.singleton) champIdsAndStats
      summonerStats = SS . M.fromList $ L.zip summonerIds championStats

      teamIds = interestingParticipants ^.. traverse . p_teamId
      num100s = L.length . L.filter (== 100) $ teamIds
      num200s = L.length . L.filter (== 200) $ teamIds
      interestingTeamId = if num100s > num200s then 100 else 200

      teamParticipants =
          sortWith (view p_participantId)
          . L.filter ((== interestingTeamId) . view p_teamId)
          $ match ^. md_participants
      teamParticipantIds = teamParticipants ^.. traverse . p_participantId
      teamParticipantIdSet = S.fromList teamParticipantIds
      teamParticipantIdentities =
          sortWith (view pi_participantId)
          . L.filter (flip S.member teamParticipantIdSet . view pi_participantId)
          $ match ^. md_participantIdentities
      teamSummonerNames = teamParticipantIdentities ^.. traverse . pi_player . p_summonerName
      teamChampions = teamParticipants ^.. traverse . p_championId

      participants = L.zip teamSummonerNames teamChampions

      victory = L.and . getZipList $ victor
  in if not (L.length interestingPIs == L.length interestingParticipants)
     then error "Got a mismatch of participants to participant ids."
     else (MatchSummary victory participants, summonerStats)

createSummonerMap :: [String] -> RiotApi (Map SummonerId Summoner)
createSummonerMap summoners = do
  summoners <- getSummonerByName summoners
  let ids = summoners ^.. traverse . s_id
  return . M.fromList $ L.zip ids summoners

createChampionMap :: RiotApi (Map ChampionId Champion)
createChampionMap = do
  championList <- getChampions
  let champions = championList ^.. cl_data . traverse
      ids = champions ^.. traverse . c_id
  return . M.fromList $ L.zip ids champions

createTeamMap :: [SummonerId] -> RiotApi (Map TeamId Team)
createTeamMap summonerIds = do
  summonerTeams <- getSummonerTeams summonerIds
  let teams = L.concat . M.elems $ summonerTeams
      teamIds = teams ^.. traverse . t_fullId
  return . M.fromList $ L.zip teamIds teams

getInterestingTeams :: [SummonerId] -> [Team] -> [Team]
getInterestingTeams summoners =
  let summonerSet = S.fromList summoners
  in L.filter (isInteresting summonerSet)
  where
    isInteresting summonerSet team =
      let playersOnTeam = S.fromList $ team ^.. t_roster . r_memberList . traverse . tmi_playerId
      in summonerSet `S.isSubsetOf` playersOnTeam

getTeamMatches :: Team -> RiotApi [MatchDetail]
getTeamMatches team = do
  let matchIds = team ^.. t_matchHistory . traverse . traverse . mhs_gameId
  mapM getMatchDetail matchIds

getInterestingMatches :: [SummonerId] -> [Team] -> RiotApi [MatchDetail]
getInterestingMatches summoners teams = do
  let summonerSet = S.fromList summoners
  matches <- L.concat <$> mapM getTeamMatches teams
  return $ L.filter (isInteresting summonerSet) matches
  where
    isInteresting summonerSet match =
      let playersInMatch = S.fromList $ match ^.. md_participantIdentities . traverse . pi_player
                                                . p_summonerId
      in (>= 4) . S.size . S.intersection summonerSet $ playersInMatch

-- TODO: Get ranked stats for each summoner.
main = do
  summoners <- $initHFlags "Scouting"
  putStrLn $ "summoners: " ++ show summoners
  putStrLn $ "Api Key: " ++ T.unpack flags_key
  riotState <- initGlobalState 1 flags_key
  env <- initEnv (stateSet riotState stateEmpty) ()
  (championMap, summonerMap, teams, matches) <- runHaxl env $ do
    championMap <- createChampionMap
    summonerMap <- createSummonerMap summoners
    teamMap <- createTeamMap (M.keys summonerMap)
    let teams = getInterestingTeams (M.keys summonerMap) (M.elems teamMap)
    matches <- getInterestingMatches (M.keys summonerMap) teams
    return (championMap, summonerMap, teams, matches)
  putStrLn . ("Summoners: " ++) . show $ summonerMap ^.. traverse . s_name
  putStrLn . ("Teams: " ++) . show $ teams ^.. traverse . t_name
  putStrLn . ("Matches: " ++) . show $ matches ^.. traverse . md_matchId
  let matchSummaries = L.map (summarizeMatch (M.keys summonerMap)) matches
  mapM_ (putStrLn . T.unpack . showMatchSummary summonerMap championMap . fst) matchSummaries
  putStrLn . T.unpack . showSummonerStats summonerMap championMap .
      mconcat . L.map snd $ matchSummaries


