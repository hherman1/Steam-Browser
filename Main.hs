module Main where
import System.Environment
import System.Console.GetOpt
import System.IO (FilePath)
import System.Directory (getDirectoryContents)

import Data.List (intersperse)
import Data.Either (rights)

import Parser

data Flag  = 	IdToName Int |
		NameToId String |
		ListNames |
		ListIds |
		Help deriving (Show, Eq)
optDescriptions :: [OptDescr Flag]
optDescriptions = [	Option ['i'] ["id"] (ReqArg NameToId "NAME") "Lookup id of game name.",
			Option ['n'] ["name"] (ReqArg (IdToName . read) "ID") "Lookup name of game id.",
			Option [] ["list-names"] (NoArg ListNames) "List names of apps.",
			Option [] ["list-ids"] (NoArg ListIds) "List ids of apps.",
			Option [] ["help"] (NoArg Help) "Print the full help menu."
			]

usage = "Usage: steambrowser [OPTION...] directories..."
helpText = "steambrowser is a tool for browsing installed steam apps and their properties.\nThe usage is \n\n" 
		++ usage
		++ "\n\n where steambrowser will read all *.acf files to form its searchable records of installed games.\n"
		++ "Typically installed appmanifests will be in \"../SteamApps/\", howevre the precise location of your SteamApps folder may vary. It can usually be found via google without much hassle, if that turns out to not hold true and people desire I will list them here in the future."
help :: String 
help = helpText ++ usageInfo usage optDescriptions

getOptions :: [String] -> IO ([Flag],[String])
getOptions s = case getOpt RequireOrder optDescriptions s of
		(flags,directories,[]) -> return (flags,directories)
		(_,_,errs) -> ioError $ userError (concat errs ++ usageInfo usage optDescriptions)

getApps :: FilePath -> IO [AppState]
getApps fp = do
	dirPaths <- getDirectoryContents fp
	let appPaths = filter isAppPath dirPaths
	appManifests <- sequence . map readFile $ map (fp++) appPaths
	return . rights $ map (parseString defaultAppState) appManifests

isAppPath :: FilePath -> Bool
isAppPath fp = (take 3 . reverse $ fp) == "fca"

processFlag :: [AppState] -> Flag -> String
processFlag _ Help = help
processFlag apps (IdToName aId) = 
	name . head $ filter ((==aId) . appId) apps
processFlag apps (NameToId aName) = 
	show . appId . head $ filter ((==aName) . name) apps
processFlag apps ListIds = 
	concat . intersperse "\n" . map (show . appId) $ apps
processFlag apps ListNames = 
	concat . intersperse "\n" . map (show . name) $ apps

main :: IO () 
main = do
	args <- getArgs
	(options,filepaths) <- getOptions args
	apps <- fmap concat . sequence . map getApps $ filepaths
	putStrLn . concat . intersperse "\n" $ map (processFlag apps) options
