module Parser (
	defaultAppState,
	AppState(..),
	parseString
) 
where

import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity (runIdentity)

data AppState = AppState {
			appId :: Int,
			universe :: Int,
			name :: String,
			stateFlags :: Int,
			installDir :: String,
			lastUpdated :: Int,
			updateResult :: Int,
			sizeOnDisk :: Int,
			buildId :: Int,
			lastOwner :: Int,
			bytesToDownload :: Int,
			bytesDownloaded :: Int,
			autoUpdateBehavior :: Int,
			allowOtherDownloadsWhileRunning :: Int,
			userConfig :: UserConfig,
			mountedDepots :: MountedDepots
		} deriving (Eq, Show)
data UserConfig = UserConfig {language :: String} deriving (Eq, Show)
data MountedDepots = MountedDepots [MountedDepot] deriving (Eq, Show)
type MountedDepot = (Int,Int)
type AppParser = Parsec String AppState

type StateModifier k = k -> AppState -> AppState

defaultAppState = AppState 0 0 "" 0 "" 0 0 0 0 0 0 0 0 0 (UserConfig "") (MountedDepots [])

stringStateModifiers :: [(String, StateModifier String)]
stringStateModifiers = [("name", (\x u -> u {name = x})),
			("installdir",(\x u -> u {installDir = x}))
			]
intStateModifiers :: [(String,StateModifier Int)]
intStateModifiers = [	("appid", (\x u -> u {appId = x})),
			("Universe",(\x u -> u {universe = x})),
			("StateFlags",(\x u -> u {stateFlags = x})),
			("LastUpdated",(\x u -> u {lastUpdated = x})),
			("UpdateResult",(\x u -> u {updateResult = x})),
			("SizeOnDisk",(\x u -> u {sizeOnDisk = x})),
			("buildid",(\x u -> u {buildId = x})),
			("LastOwner",(\x u -> u {lastOwner = x})),
			("BytesToDownload",(\x u -> u {bytesToDownload = x})),
			("BytesDownloaded",(\x u -> u {bytesDownloaded = x})),
			("AutoUpdateBehavior",(\x u -> u {autoUpdateBehavior = x})),
			("AllowOtherDownloadsWhileRunning",(\x u -> u {allowOtherDownloadsWhileRunning = x}))
			]
			

quotation = char '"'

quotedWord :: AppParser String
quotedWord = many $ satisfy (/='"')

parseQuote :: AppParser a -> AppParser a
parseQuote u = between quotation quotation u

parseLine :: AppParser ()
parseLine = do
	label <- phrase
	value <- phrase
	modifyState $ case lookup label stringStateModifiers of
		Just f -> f value
		Nothing -> case lookup label intStateModifiers of
			Just f -> f (read value :: Int)
			Nothing -> id
	where phrase = spaces >> parseQuote quotedWord

parseLoop :: AppParser ()
parseLoop = (try parseLine) <|> nextLine
	where nextLine = manyTill anyToken newline >> return ()

parseBlock :: AppParser AppState
parseBlock = manyTill parseLoop eof >> getState

parseString :: AppState -> String -> (Either ParseError AppState)
parseString u = runIdentity . runParserT parseBlock u "parsing steam app string"
