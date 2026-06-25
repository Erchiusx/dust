module Main where

import Control.Concurrent.MVar
import Control.Exception (catch)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as Strict
import Data.ByteString.Lazy qualified as Lazy
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import DustUp.Artifacts.Type
import DustUp.Engine
import DustUp.Initialize
import DustUp.Type
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , defaultWebAppSettings
  , ssMaxAge
  , staticApp
  )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.Entropy (getEntropy)
import System.Random (newStdGen)
import Text.Read (readMaybe)
import WaiAppStatic.Types (MaxAge (NoStore))

data Client'Command
  = Join (Maybe Text)
  | Set'Loadout Player'Loadout
  | Submit'Movement Movement
  | Get'Status
  | Ping

instance FromJSON Client'Command where
  parseJSON = withObject "ClientCommand" $ \object -> do
    command <- object .: "type"
    case command :: Text of
      "join" -> Join <$> object .:? "token"
      "loadout" ->
        Set'Loadout
          <$> ( Player'Loadout
                  <$> (object .: "columnOne" >>= parseCardID)
                  <*> (object .: "columnTwo" >>= parseCardID)
                  <*> (object .: "columnThree" >>= parseCardID)
              )
      "movement" -> Submit'Movement <$> (object .: "movement" >>= parseMovement)
      "status" -> pure Get'Status
      "ping" -> pure Ping
      _ -> fail "unknown command type"

data Seat = Seat
  { player'id :: Game'ID
  , token :: Text
  , loadout :: Maybe Player'Loadout
  }

data Server'State = Server'State
  { seats :: [Seat]
  , engine :: Maybe Engine'Result
  }

main :: IO ()
main = do
  port <- maybe 9160 id . (>>= readMaybe) <$> lookupEnv "PORT"
  state <- newMVar $ Server'State [] Nothing
  let assets =
        staticApp $
          (defaultFileServerSettings "assets"){ssMaxAge = NoStore}
      client =
        staticApp $
          (defaultWebAppSettings "client"){ssMaxAge = NoStore}
  putStrLn $ "DustUp server listening on http://0.0.0.0:" <> show port
  Warp.run port $ application state assets client

application
  :: MVar Server'State
  -> Wai.Application
  -> Wai.Application
  -> Wai.Application
application state assets client request respond =
  case Wai.pathInfo request of
    ["ws"] ->
      websocketsOr
        WS.defaultConnectionOptions
        (serveConnection state)
        client
        request
        respond
    "assets" : path ->
      assets request{Wai.pathInfo = path} respond
    [] ->
      client request{Wai.pathInfo = ["index.html"]} respond
    _ ->
      client request respond

serveConnection :: MVar Server'State -> WS.ServerApp
serveConnection state pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (pure ()) $ do
    firstMessage <- receiveCommand connection
    case firstMessage of
      Left err -> sendError connection err
      Right (Join suppliedToken) -> do
        identity <- claimSeat state suppliedToken
        case identity of
          Left err -> sendError connection err
          Right seat -> do
            sendWelcome connection seat
            sendStatus connection =<< readMVar state
            commandLoop state seat connection
              `catch` ignoreConnectionClose
      Right _ ->
        sendError connection "first command must be join"

ignoreConnectionClose :: WS.ConnectionException -> IO ()
ignoreConnectionClose _ = pure ()

commandLoop
  :: MVar Server'State
  -> Seat
  -> WS.Connection
  -> IO ()
commandLoop state seat connection = do
  command <- receiveCommand connection
  case command of
    Left err -> sendError connection err
    Right (Join _) -> sendError connection "already joined"
    Right (Set'Loadout loadout) ->
      updateLoadout state seat loadout >>= sendResult connection
    Right (Submit'Movement movement) ->
      submitMovement state seat movement >>= sendResult connection
    Right Get'Status ->
      Right <$> readMVar state >>= sendResult connection
    Right Ping ->
      sendJSON connection $ object ["type" .= ("pong" :: Text)]
  commandLoop state seat connection

claimSeat
  :: MVar Server'State
  -> Maybe Text
  -> IO (Either Text Seat)
claimSeat state suppliedToken =
  modifyMVar state $ \server ->
    case suppliedToken >>= findSeatByToken server of
      Just seat -> pure (server, Right seat)
      Nothing
        | suppliedToken /= Nothing ->
            pure (server, Left "unknown reconnect token")
        | length server.seats >= 2 ->
            pure (server, Left "this match already has two players")
        | otherwise -> do
            generatedToken <- randomToken
            let seat =
                  Seat
                    { player'id = length server.seats
                    , token = generatedToken
                    , loadout = Nothing
                    }
            pure
              ( server{seats = server.seats <> [seat]}
              , Right seat
              )

findSeatByToken :: Server'State -> Text -> Maybe Seat
findSeatByToken server wanted =
  find ((== wanted) . (.token)) server.seats

updateLoadout
  :: MVar Server'State
  -> Seat
  -> Player'Loadout
  -> IO (Either Text Server'State)
updateLoadout state seat selectedLoadout =
  modifyMVar state $ \server -> do
    case server.engine of
      Just _ ->
        pure (server, Left "loadouts are locked after the match starts")
      Nothing -> do
        let updatedSeats =
              map
                ( \candidate ->
                    if candidate.player'id == seat.player'id
                      then candidate{loadout = Just selectedLoadout}
                      else candidate
                )
                server.seats
            updated = server{seats = updatedSeats}
        initialized <- initializeIfReady updated
        case initialized of
          Left err -> pure (server, Left err)
          Right next -> pure (next, Right next)

initializeIfReady
  :: Server'State
  -> IO (Either Text Server'State)
initializeIfReady server =
  case server.seats of
    [firstSeat, secondSeat]
      | Just firstLoadout <- firstSeat.loadout
      , Just secondLoadout <- secondSeat.loadout -> do
          generator <- newStdGen
          pure $
            case
              initializeGameWithFirstPlayer
                generator
                0
                firstLoadout
                secondLoadout
            of
              Left err -> Left $ Text.pack $ show err
              Right result -> Right server{engine = Just result}
    _ -> pure $ Right server

submitMovement
  :: MVar Server'State
  -> Seat
  -> Movement
  -> IO (Either Text Server'State)
submitMovement state seat movement
  | movementPlayer movement /= seat.player'id =
      pure $ Left "movement player does not match authenticated seat"
  | otherwise =
      modifyMVar state $ \server ->
        case server.engine of
          Nothing ->
            pure (server, Left "the match has not started")
          Just result ->
            case advanceGame result movement of
              Left err -> pure (server, Left err)
              Right next ->
                let updated = server{engine = Just next}
                 in pure (updated, Right updated)

advanceGame
  :: Engine'Result
  -> Movement
  -> Either Text Engine'Result
advanceGame result movement =
  case result of
    Engine'Awaiting _ resume ->
      Right $ resume movement
    Engine'Completed game ->
      Right $ runGameMovement movement game
    Engine'Failed err _ ->
      Left $ Text.pack $ show err

movementPlayer :: Movement -> Game'ID
movementPlayer = \case
  Pass player -> player
  Reroll player _ -> player
  DustUp player _ _ -> player
  Attack player _ _ -> player
  Defend player _ _ -> player
  Activate player _ _ _ -> player
  Select player _ _ -> player
  Choose'Option player _ _ -> player

receiveCommand :: WS.Connection -> IO (Either Text Client'Command)
receiveCommand connection = do
  payload <- WS.receiveData connection
  pure $ firstText $ eitherDecode (payload :: Lazy.ByteString)

firstText :: Either String a -> Either Text a
firstText = \case
  Left err -> Left $ Text.pack err
  Right value -> Right value

sendWelcome :: WS.Connection -> Seat -> IO ()
sendWelcome connection seat =
  sendJSON connection $
    object
      [ "type" .= ("welcome" :: Text)
      , "player" .= seat.player'id
      , "role"
          .= if seat.player'id == 0
            then ("first" :: Text)
            else "second"
      , "token" .= seat.token
      ]

sendResult
  :: WS.Connection
  -> Either Text Server'State
  -> IO ()
sendResult connection = \case
  Left err -> sendError connection err
  Right server -> sendStatus connection server

sendStatus :: WS.Connection -> Server'State -> IO ()
sendStatus connection server =
  sendJSON connection $
    object
      [ "type" .= ("status" :: Text)
      , "players"
          .= [ object
                [ "player" .= seat.player'id
                , "ready" .= maybe False (const True) seat.loadout
                ]
             | seat <- server.seats
             ]
      , "match" .= matchStatus server.engine
      ]

matchStatus :: Maybe Engine'Result -> Value
matchStatus = \case
  Nothing -> object ["state" .= ("waiting-for-loadouts" :: Text)]
  Just (Engine'Completed game) ->
    object
      [ "state" .= ("ready" :: Text)
      , "round" .= game.time.round
      , "player" .= game.time.player
      , "phase" .= show game.time.phase
      , "winners" .= game.winners
      ]
  Just (Engine'Awaiting options _) ->
    object
      [ "state" .= ("awaiting-input" :: Text)
      , "request" .= show options
      ]
  Just (Engine'Failed err _) ->
    object
      [ "state" .= ("failed" :: Text)
      , "error" .= show err
      ]

sendError :: WS.Connection -> Text -> IO ()
sendError connection message =
  sendJSON connection $
    object
      [ "type" .= ("error" :: Text)
      , "message" .= message
      ]

sendJSON :: WS.Connection -> Value -> IO ()
sendJSON connection =
  WS.sendTextData connection . encode

randomToken :: IO Text
randomToken =
  Text.pack . concatMap paddedHex . Strict.unpack <$> getEntropy 32

paddedHex :: Word8 -> String
paddedHex value =
  let encoded = showHex value ""
   in replicate (2 - length encoded) '0' <> encoded

parseCardID :: Text -> Parser Card'ID
parseCardID value =
  case Text.uncons value of
    Just ('C', number) | not (Text.null number) ->
      pure $ Card'ID Core number
    Just ('A', number) | not (Text.null number) ->
      pure $ Card'ID Alternate number
    _ -> fail "card ID must look like C001 or A01"

parseMovement :: Value -> Parser Movement
parseMovement = withObject "Movement" $ \object -> do
  kind <- object .: "type"
  player <- object .: "player"
  case kind :: Text of
    "pass" -> pure $ Pass player
    "reroll" -> Reroll player <$> object .: "dice"
    "dustUp" ->
      DustUp player
        <$> object .: "artifact"
        <*> (object .: "cost" >>= parseDustUpCost)
    "attack" ->
      Attack player
        <$> object .: "target"
        <*> object .: "die"
    "defend" ->
      Defend player
        <$> object .: "request"
        <*> object .:? "die"
    "activate" ->
      Activate player
        <$> object .: "artifact"
        <*> object .: "ability"
        <*> object .:? "costs" .!= []
    "select" ->
      Select player
        <$> object .: "request"
        <*> object .: "targets"
    "chooseOption" ->
      Choose'Option player
        <$> object .: "request"
        <*> object .: "option"
    _ -> fail "unknown movement type"

parseDustUpCost :: Value -> Parser DustUp'Cost
parseDustUpCost = withObject "DustUpCost" $ \object -> do
  kind <- object .: "type"
  case kind :: Text of
    "seal" -> pure Dust'Seal
    "thoughtDie" -> Thought'Die <$> object .: "die"
    _ -> fail "unknown dust-up cost"
