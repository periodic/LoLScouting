module Parsers where

import Control.Lens
import Control.Monad
import Data.Map as M
import Data.Monoid
import Data.Text
import RiotApi.Types

data PlayerRecord = PlayerRecord
  { summonerId :: SummonerId,
    championId :: ChampionId,
    queueType :: Text,
    duration :: Int,
    kills :: Int,
    deaths :: Int,
    assists :: Int,
    gamesTotal :: Int,
    gamesWon :: Int,
    creepScore :: Int,
    goldEarned :: Int,
    towersKilled :: Int,
    dragonsKilled :: Int,
    baronsKilled :: Int,
    greenWardsPurchased :: Int,
    pinkWardsPurchased :: Int,
    wardsPlaced :: Int } deriving (Show)

getStatsFromGame :: MatchDetail -> [PlayerRecord]
getStatsFromGame match = do
  let duration = match ^. md_matchDuration
      queueType = match ^. md_queueType

  team <- match ^. md_teams
  let baronKills = team ^. mt_baronKills
      dragonKills = team ^. mt_dragonKills
      towerKills = team ^. mt_dragonKills
      teamId = team ^. mt_teamId
      winner = team ^. mt_winner

  participant <- match ^. md_participants
  identity <- match ^. md_participantIdentities

  guard $ participant ^. p_teamId == teamId
  guard $ identity ^. pi_participantId == participant ^. p_participantId

  let summonerId = identity ^. pi_player . p_summonerId
      championId = participant ^. p_championId
      stats = participant ^. p_stats
      kills = stats ^. ps_kills
      deaths = stats ^. ps_deaths
      assists = stats ^. ps_assists
      gold = stats ^. ps_goldEarned
      minions = stats ^. ps_minionsKilled
      greenWards = stats ^. ps_sightWardsBoughtInGame
      pinkWards = stats ^. ps_visionWardsBoughtInGame
      wardsPlaced = stats ^. ps_wardsPlaced
  [PlayerRecord
    summonerId
    championId
    queueType
    duration
    kills
    deaths
    assists
    1 -- gamesTotal
    (if winner then 1 else 0)
    minions
    gold
    towerKills
    dragonKills
    baronKills
    greenWards
    pinkWards
    wardsPlaced]




