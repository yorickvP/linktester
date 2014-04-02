module Main where
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP
import Network.URI
import Control.Monad
import Text.HTML.TagSoup
import System.Environment
import System.Console.GetOpt
import Network.Stream ( Result )

getResponseHeaders :: Result (Response ty) -> IO [Header]
getResponseHeaders (Left err) = fail (show err)
getResponseHeaders (Right r)  = return (rspHeaders r)

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

extractHrefs = map (parseURIReference) . map (fromAttrib "href") . filter (isTagOpenName "a")
samepage x y = uriScheme x == uriScheme y &&
				uriAuthority x == uriAuthority y &&
				uriPath x == uriPath y &&
				uriQuery x == uriQuery y
stripFragment :: URI -> URI
stripFragment x = URI
	{ uriScheme    = uriScheme x
	, uriAuthority = uriAuthority x
	, uriPath      = uriPath x
	, uriQuery     = uriQuery x
	, uriFragment  = "" }
anyPrefix :: [String] -> URI -> Bool
anyPrefix list url = foldr (||) False (map (flip isPrefixOf $ show url) list)
shouldFilter :: Options -> URI -> Bool
shouldFilter opts url = 
	(not $ anyPrefix (optWithout opts) url) &&
	(onlyFollow opts url || (anyPrefix (optWith opts) url)) &&
	((uriScheme url) == "http:")
filterURLs :: Options -> [URI] -> [URI]
filterURLs = filter . shouldFilter
onlyFollow :: Options -> URI -> Bool
onlyFollow = anyPrefix . optFollow

type TBDList = [(URI, URI)]
type DoneList = Map URI (ResponseCode, [URI])
doneURI (page, code, chain) = page

getHrefsFromUrl :: Options -> URI -> IO (ResponseCode, TBDList)
getHrefsFromUrl opts x = do
	req <- simpleHTTP (getRequest $ show x)
	code <- getResponseCode req
	case code of
		(2, _, _) -> do
			src <- getResponseBody req
			let parsed = parseTags src
			let extractedURLS = catMaybes $ map (liftM $ flip relativeTo x) (extractHrefs parsed)
			return (code, if onlyFollow opts x then [] else map expandChain $ filterURLs opts extractedURLS)
		(3, 0, 1) -> do -- redirect
			headers <- getResponseHeaders req
			case (lookupHeader HdrLocation headers) >>= parseURIReference >>= (return . (flip relativeTo x)) of
				Nothing -> return (code, [])
				Just loc -> if (shouldFilter opts loc) then
					return (code, [expandChain loc])
					else return (code, [])
		otherwise -> return (code, [])
	where expandChain u = (u, x)
showNum (a,b,c) = show a ++ show b ++ show c

doPages :: Options -> DoneList -> TBDList -> IO DoneList
doPages opts alreadyDone []              = return alreadyDone
doPages opts alreadyDone ((page, referral):toBeDone) = do
	case (Map.lookup page' alreadyDone) of
		Nothing -> do
			putStr $ "requesting " ++ show page
			(code, urls) <- getHrefsFromUrl opts page
			putStrLn $ " " ++ showNum code
			doPages opts (Map.insert page' (code, [referral]) alreadyDone) (toBeDone ++ urls)
		Just doneRecord ->
			doPages opts (Map.update (addRef referral) page' alreadyDone) toBeDone
	where
		addRef ref (code, refs) = Just (code, ref:refs)
		page' = stripFragment page

printNicely :: Options -> [(URI, (ResponseCode, [URI]))] -> String
printNicely _ [] = ""
printNicely opts ((x, (code@(a, b, c), referrals)):rest)
	| (a /= 2 && a /= 3) || (onlyFollow opts x) =
		showNum code ++ " " ++ show x ++ " linked from " ++ show referrals ++ "\n"
			++ printNicely opts rest
	| otherwise = printNicely opts rest

data Options = Options
 { optWithout :: [String],
   optWith :: [String],
   optFollow :: [String] }
 deriving Show
defaultOptions = Options
 { optWithout = [], optWith = [], optFollow = [] }
main = do 
	(options, args) <- opts =<< getArgs
	if length args < 1 then
		putStrLn "See linktester --help"
	else return ()
	let (url:xs) = args
	let Just start = parseURI url
	let options' = options { optWith = fixWith url $ optWith options }
	hrefs <- doPages options' Map.empty [(start, start)]
	putStrLn "OUTPUT FOLLOWING"
	putStrLn $ printNicely options (Map.toList hrefs)
	where
		fixWith url [] = [url] -- add the starting url if no -o specified
		fixWith url x  = x

options :: [OptDescr (Options -> Options)]
options = [ Option ['w'] ["filter"] (ReqArg (\f opts ->
					opts {optWithout = optWithout opts ++ [f] }) "url")
				"don't go into urls starting with this",
			Option ['o'] ["only"] (ReqArg (\f opts ->
					opts {optWith = optWith opts ++ [f] }) "url")
				"do go into urls starting with this",
			Option ['f'] ["follow"] (ReqArg (\f opts ->
					opts {optFollow = optFollow opts ++ [f] }) "url")
				"check urls starting with this, but don't also check links in them"
		]
opts :: [String] -> IO (Options, [String])
opts args = 
	case getOpt Permute options args of
		(flags, nonOpts, [])     -> return (foldl (flip id) defaultOptions flags, nonOpts)
		(_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options
	where header = "Usage: linktester link [OPTION...]\n" ++
					"example: linktester http://www.google.com/" ++ 
					" -f http://youtube.com/ -w http://google.com/jobs"
