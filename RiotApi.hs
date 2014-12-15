{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module RiotApi where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.RateLimit
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Map (Map, elems)
import Data.Text (Text)
import Data.Time.Units
import GHC.Generics (Generic)
import Network.Wreq

type RiotApiKey = String
type UnvalidatedSummonerName = String
type SummonerId = Integer
type SummonerName = Text
type ChampionId = Integer
type ChampionName = Text
type MatchId = Integer
type TeamId = String

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

logMsg :: String -> RiotApi ()
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

--------------------------------------------------------------------------------
--  GetSummonerByName
--------------------------------------------------------------------------------

data Summoner = Summoner
  { _s_id :: Integer -- | Summoner ID.
  , _s_name :: SummonerName -- | Summoner name.
  , _s_profileIconId :: Int -- | ID of the summoner icon associated with the summoner.
  , _s_revisionDate :: Integer -- | Date summoner was last modified specified as epoch milliseconds.
  , _s_summonerLevel :: Integer -- | Summoner level associated with the summoner.
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Summoner)
makeClassy ''Summoner

getSummonerByName :: [UnvalidatedSummonerName] -> RiotApi [Summoner]
getSummonerByName names =
  let path = "/v1.4/summoner/by-name/"
      summonerNames = intercalate "," names
      url = concat [riotBaseApiUrl, path, summonerNames]
  in do
    resp <- asJSON =<< getApi defaults url :: RiotApi (Response (Map String Summoner))
    return $ resp ^. responseBody . to elems

--------------------------------------------------------------------------------
--  GetTeamBySummoner
--------------------------------------------------------------------------------

data TeamMemberInfo = TeamMemberInfo
  { _tmi_inviteDate :: Integer -- | Date that team member was invited to team specified as epoch milliseconds.
  , _tmi_joinDate :: Integer -- | Date that team member joined team specified as epoch milliseconds.
  , _tmi_playerId :: SummonerId -- |
  , _tmi_status :: String -- |
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''TeamMemberInfo)
makeClassy ''TeamMemberInfo

data TeamStatDetail = TeamStatDetail
  { _tsd_averageGamesPlayed :: Int -- |
  , _tsd_losses :: Int -- |
  , _tsd_teamStatType :: String -- |
  , _tsd_wins :: Int
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''TeamStatDetail)
makeClassy ''TeamStatDetail

data Roster = Roster
  { _r_memberList :: [TeamMemberInfo]
  , _r_ownerId :: Integer
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Roster)
makeClassy ''Roster

data MatchHistorySummary = MatchHistorySummary
  { _mhs_assists :: Int -- |
  , _mhs_date :: Integer -- | Date that match was completed specified as epoch milliseconds.
  , _mhs_deaths :: Int -- |
  , _mhs_gameId :: MatchId -- |
  , _mhs_gameMode :: String -- |
  , _mhs_invalid :: Bool -- |
  , _mhs_kills :: Int -- |
  , _mhs_mapId :: Int -- |
  , _mhs_opposingTeamKills :: Int -- |
  , _mhs_opposingTeamName :: String -- |
  , _mhs_win :: Bool -- |
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''MatchHistorySummary)
makeClassy ''MatchHistorySummary

data Team = Team
  { _t_createDate :: Integer -- | Date that team was created specified as epoch milliseconds.
  , _t_fullId :: TeamId -- |
  , _t_lastGameDate :: Maybe Integer -- | Date that last game played by team ended specified as epoch milliseconds.
  , _t_lastJoinDate :: Integer -- | Date that last member joined specified as epoch milliseconds.
  , _t_lastJoinedRankedTeamQueueDate :: Maybe Integer -- | Date that team last joined the ranked team queue specified as epoch milliseconds.
  , _t_matchHistory :: Maybe [MatchHistorySummary]
  , _t_modifyDate :: Integer -- | Date that team was last modified specified as epoch milliseconds.
  , _t_name :: String -- |
  , _t_roster :: Roster -- |
  , _t_secondLastJoinDate :: Maybe Integer -- | Date that second to last member joined specified as epoch milliseconds.
  , _t_status :: String -- |
  , _t_tag :: String -- |
  , _t_teamStatDetails :: [TeamStatDetail]
  , _t_thirdLastJoinDate :: Maybe Integer -- | Date that third to last member joined specified as epoch milliseconds.
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Team)
makeClassy ''Team

getSummonerTeams :: [SummonerId] -> RiotApi (Map Text [Team])
getSummonerTeams summonerIds =
  let path = "/v2.4/team/by-summoner/"
      summonerNames = intercalate "," . map show $ summonerIds
      url = concat [riotBaseApiUrl, path, summonerNames]
  in do
    resp <- asJSON =<< getApi defaults url
    return $ resp ^. responseBody

--------------------------------------------------------------------------------
--  GetMatchDetail
--------------------------------------------------------------------------------

data BannedChampion = BannedChampion
  { _bc_championId :: ChampionId -- |Banned champion ID
  , _bc_pickTurn :: Int -- |Turn during which the champion was banned
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''BannedChampion)
makeClassy ''BannedChampion

data Player = Player
  { _p_matchHistoryUri :: String -- |Match history URI
  , _p_profileIcon :: Int -- |Profile icon ID
  , _p_summonerId :: Integer -- |Summoner ID
  , _p_summonerName :: SummonerName -- |Summoner name
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Player)
makeClassy ''Player

data ParticipantStats = ParticipantStats
  { _ps_assists :: Integer -- |Number of assists
  , _ps_champLevel :: Integer -- |Champion level achieved
  , _ps_combatPlayerScore :: Maybe Integer -- |If game was a dominion game, player's combat score, otherwise 0
  , _ps_deaths :: Integer -- |Number of deaths
  , _ps_doubleKills :: Integer -- |Number of double kills
  , _ps_firstBloodAssist :: Bool -- |Flag indicating if participant got an assist on first blood
  , _ps_firstBloodKill :: Bool -- |Flag indicating if participant got first blood
  , _ps_firstInhibitorAssist :: Bool -- |Flag indicating if participant got an assist on the first inhibitor
  , _ps_firstInhibitorKill :: Bool -- |Flag indicating if participant destroyed the first inhibitor
  , _ps_firstTowerAssist :: Bool -- |Flag indicating if participant got an assist on the first tower
  , _ps_firstTowerKill :: Bool -- |Flag indicating if participant destroyed the first tower
  , _ps_goldEarned :: Integer -- |Gold earned
  , _ps_goldSpent :: Integer -- |Gold spent
  , _ps_inhibitorKills :: Integer -- |Number of inhibitor kills
  , _ps_item0 :: Integer -- |First item ID
  , _ps_item1 :: Integer -- |Second item ID
  , _ps_item2 :: Integer -- |Third item ID
  , _ps_item3 :: Integer -- |Fourth item ID
  , _ps_item4 :: Integer -- |Fifth item ID
  , _ps_item5 :: Integer -- |Sixth item ID
  , _ps_item6 :: Integer -- |Seventh item ID
  , _ps_killingSprees :: Integer -- |Number of killing sprees
  , _ps_kills :: Integer -- |Number of kills
  , _ps_largestCriticalStrike :: Integer -- |Largest critical strike
  , _ps_largestKillingSpree :: Integer -- |Largest killing spree
  , _ps_largestMultiKill :: Integer -- |Largest multi kill
  , _ps_magicDamageDealt :: Integer -- |Magical damage dealt
  , _ps_magicDamageDealtToChampions :: Integer -- |Magical damage dealt to champions
  , _ps_magicDamageTaken :: Integer -- |Magic damage taken
  , _ps_minionsKilled :: Integer -- |Minions killed
  , _ps_neutralMinionsKilled :: Integer -- |Neutral minions killed
  , _ps_neutralMinionsKilledEnemyJungle :: Integer -- |Neutral jungle minions killed in the enemy team's jungle
  , _ps_neutralMinionsKilledTeamJungle :: Integer -- |Neutral jungle minions killed in your team's jungle
  , _ps_nodeCapture :: Maybe Integer -- |If game was a dominion game, number of node captures
  , _ps_nodeCaptureAssist :: Maybe Integer -- |If game was a dominion game, number of node capture assists
  , _ps_nodeNeutralize :: Maybe Integer -- |If game was a dominion game, number of node neutralizations
  , _ps_nodeNeutralizeAssist :: Maybe Integer -- |If game was a dominion game, number of node neutralization assists
  , _ps_objectivePlayerScore :: Maybe Integer -- |If game was a dominion game, player's objectives score, otherwise 0
  , _ps_pentaKills :: Integer -- |Number of penta kills
  , _ps_physicalDamageDealt :: Integer -- |Physical damage dealt
  , _ps_physicalDamageDealtToChampions :: Integer -- |Physical damage dealt to champions
  , _ps_physicalDamageTaken :: Integer -- |Physical damage taken
  , _ps_quadraKills :: Integer -- |Number of quadra kills
  , _ps_sightWardsBoughtInGame :: Integer -- |Sight wards purchased
  , _ps_teamObjective :: Maybe Integer -- |If game was a dominion game, number of completed team objectives (i.e., quests)
  , _ps_totalDamageDealt :: Integer -- |Total damage dealt
  , _ps_totalDamageDealtToChampions :: Integer -- |Total damage dealt to champions
  , _ps_totalDamageTaken :: Integer -- |Total damage taken
  , _ps_totalHeal :: Integer -- |Total heal amount
  , _ps_totalPlayerScore :: Maybe Integer -- |If game was a dominion game, player's total score, otherwise 0
  , _ps_totalScoreRank :: Maybe Integer -- |If game was a dominion game, team rank of the player's total score (e.g., 1-5)
  , _ps_totalTimeCrowdControlDealt :: Integer -- |Total dealt crowd control time
  , _ps_totalUnitsHealed :: Integer -- |Total units healed
  , _ps_towerKills :: Integer -- |Number of tower kills
  , _ps_tripleKills :: Integer -- |Number of triple kills
  , _ps_trueDamageDealt :: Integer -- |True damage dealt
  , _ps_trueDamageDealtToChampions :: Integer -- |True damage dealt to champions
  , _ps_trueDamageTaken :: Integer -- |True damage taken
  , _ps_unrealKills :: Integer -- |Number of unreal kills
  , _ps_visionWardsBoughtInGame :: Integer -- |Vision wards purchased
  , _ps_wardsKilled :: Integer -- |Number of wards killed
  , _ps_wardsPlaced :: Integer -- |Number of wards placed
  , _ps_winner :: Bool -- |Flag indicating whether or not the participant won
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''ParticipantStats)
makeClassy ''ParticipantStats

data MatchTeam = MatchTeam
  { _mt_bans :: [BannedChampion] -- | If game was draft mode, contains banned champion data, otherwise null
  , _mt_baronKills :: Int -- |Number of times the team killed baron
  , _mt_dominionVictoryScore :: Maybe Integer -- |If game was a dominion game, specifies the points the team had at game end, otherwise null
  , _mt_dragonKills :: Int -- |Number of times the team killed dragon
  , _mt_firstBaron :: Bool -- |Flag indicating whether or not the team got the first baron kill
  , _mt_firstBlood :: Bool -- |Flag indicating whether or not the team got first blood
  , _mt_firstDragon :: Bool -- |Flag indicating whether or not the team got the first dragon kill
  , _mt_firstInhibitor :: Bool -- |Flag indicating whether or not the team destroyed the first inhibitor
  , _mt_firstTower :: Bool -- |Flag indicating whether or not the team destroyed the first tower
  , _mt_inhibitorKills :: Int -- |Number of inhibitors the team destroyed
  , _mt_teamId :: Int -- |Team ID
  , _mt_towerKills :: Int -- |Number of towers the team destroyed
  , _mt_vilemawKills :: Int -- |Number of times the team killed vilemaw
  , _mt_winner :: Bool -- |Flag indicating whether or not the team won
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''MatchTeam)
makeClassy ''MatchTeam

data ParticipantIdentity = ParticipantIdentity
  { _pi_participantId :: Int -- |Participant ID
  , _pi_player :: Player -- |Player information
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''ParticipantIdentity)
makeClassy ''ParticipantIdentity

data Participant = Participant
  { _p_championId :: ChampionId -- |Champion ID
  -- , _p_masteries :: List -- |[Mastery] List of mastery information
  , _p_participantId :: Int -- |Participant ID
  -- , _p_runes :: List -- |[Rune]  List of rune information
  , _p_spell1Id :: Int -- |First summoner spell ID
  , _p_spell2Id :: Int -- |Second summoner spell ID
  , _p_stats :: ParticipantStats -- |Participant statistics
  , _p_teamId :: Int -- |Team ID
  --, _p_timeline :: ParticipantTimeline -- |Timeline data. Delta fields refer to values for the specified period (e.g., the gold per minute over the first 10 minutes of the game versus the second 20 minutes of the game. Diffs fields refer to the deltas versus the calculated lane opponent(s).
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Participant)
makeClassy ''Participant

data MatchDetail = MatchDetail
  { _md_mapId :: Int -- |Match map ID
  , _md_matchCreation :: Integer -- |Match creation time. Designates when the team select lobby is created and/or the match is made through match making, not when the game actually starts.
  , _md_matchDuration :: Integer -- |Match duration
  , _md_matchId :: MatchId -- |ID of the match
  , _md_matchMode :: String -- |Match mode (legal values: CLASSIC, ODIN, ARAM, TUTORIAL, ONEFORALL, ASCENSION, FIRSTBLOOD)
  , _md_matchType :: String -- |Match type (legal values: CUSTOM_GAME, MATCHED_GAME, TUTORIAL_GAME)
  , _md_matchVersion :: String -- |Match version
  , _md_participantIdentities :: [ParticipantIdentity] -- |Participant identity information
  , _md_participants :: [Participant] -- |Participant information
  , _md_platformId :: String -- |Platform ID of the match
  , _md_queueType :: String -- |Match queue type (legal values: CUSTOM, NORMAL_5x5_BLIND, RANKED_SOLO_5x5, RANKED_PREMADE_5x5, BOT_5x5, NORMAL_3x3, RANKED_PREMADE_3x3, NORMAL_5x5_DRAFT, ODIN_5x5_BLIND, ODIN_5x5_DRAFT, BOT_ODIN_5x5, BOT_5x5_INTRO, BOT_5x5_BEGINNER, BOT_5x5_INTERMEDIATE, RANKED_TEAM_3x3, RANKED_TEAM_5x5, BOT_TT_3x3, GROUP_FINDER_5x5, ARAM_5x5, ONEFORALL_5x5, FIRSTBLOOD_1x1, FIRSTBLOOD_2x2, SR_6x6, URF_5x5, BOT_URF_5x5, NIGHTMARE_BOT_5x5_RANK1, NIGHTMARE_BOT_5x5_RANK2, NIGHTMARE_BOT_5x5_RANK5, ASCENSION_5x5, HEXAKILL)
  , _md_region :: String -- |Region where the match was played
  , _md_season :: String -- |Season match was played (legal values: PRESEASON3, SEASON3, PRESEASON2014, SEASON2014)
  , _md_teams :: [MatchTeam] -- | Team information
  -- _md_, timeline :: Timeline -- |Match timeline data. Not included by default.
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''MatchDetail)
makeClassy ''MatchDetail

getMatchDetail :: MatchId -> RiotApi MatchDetail
getMatchDetail matchId =
  let path = "/v2.2/match/"
      url = concat [riotBaseApiUrl, path, show matchId]
  in do
    resp <- asJSON =<< getApi defaults url
    return $ resp ^. responseBody

--------------------------------------------------------------------------------
--  GetChampions
--------------------------------------------------------------------------------

data Champion = Champion
  { _c_id :: ChampionId -- |
  -- , _c_image :: ImageDto -- |
  -- , _c_info :: InfoDto -- |
  , _c_key :: String -- |
  , _c_name :: ChampionName -- |
  , _c_title :: String -- |
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Champion)
makeClassy ''Champion

data ChampionList = ChampionList
  { _cl_data :: Map String Champion
  -- , _cl_format :: String
  -- , _cl_keys :: Map String String
  , _cl_type :: String -- |
  , _cl_version :: String -- |
  } deriving (Generic, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''ChampionList)
makeClassy ''ChampionList

getChampions :: RiotApi ChampionList
getChampions =
  let path = "/v1.2/champion"
      url = concat [riotBaseStaticUrl, path]
  in do
    resp <- asJSON =<< getApi defaults url
    return $ resp ^. responseBody