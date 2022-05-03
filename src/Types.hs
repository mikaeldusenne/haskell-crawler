{-# LANGUAGE LambdaCase #-}
module Types where

import Control.Monad.Trans.State
import Text.HTML.TagSoup
import Data.Digest.Pure.SHA
import System.FilePath.Posix
import Control.Monad.IO.Class (liftIO)
import Network.Curl
import List
import Misc
import System.Directory
import Control.Monad
import Text.StringLike

type Pathl = [String]
type T = (Pathl, String)
type T' = PPM (Either String [(String, String)])

type ErrorDetails = Either (String, String) ()

type PPM a = StateT Config IO a

type Cache = [(String, String)]

type SHAString = String
type Path = String
-- type URLString = String

data UrlWithDest = UrlWithDest {url::URLString, dest::FilePath}
  deriving (Show)

defaultUrlWithDest = UrlWithDest {url="", dest=""}

data Config = Config {
  download_folder :: Path,
  base :: URLString,
  login_path :: Maybe String,
  login_arg_login :: String,
  login_arg_pass :: String,
  -- sha256sumsfile :: String,
  urlsfile :: Path,
  login_needed_tag :: Tag String,
  login_env_prefix :: String,
  -- alreadies :: [(SHAString, String)],
  alreadies_urls :: [(URLString, Path)]
  }
  deriving (Show)

defaultconfig = Config {
  download_folder = "scrapper_downloads",
  base = "https://hello-world.com/",
  login_path = Nothing, --"login.php",
  login_arg_login = "member_login",
  login_arg_pass = "member_pass",
  -- sha256sumsfile = "sha256sums.txt",
  urlsfile = "urls.txt",
  login_needed_tag = TagOpen "button" [("aria-label","Please sign in")],
  login_env_prefix = "scrapper_",
  -- alreadies = [],
  alreadies_urls = []
  }

-- 
createConfig cfg = do
  -- shas <- map read . lines <$> readFile (download_folder cfg </> sha256sumsfile cfg)
  let dlf = download_folder cfg
  createDirectoryIfMissing True dlf
  
  let shf = dlf </> urlsfile cfg
  (not <$> doesFileExist shf) >>= (`when` writeFile shf "")
  urls <- map read . lines <$> readFile (download_folder cfg </> urlsfile cfg)
  return cfg{alreadies_urls=urls}
  
handle_url_cache :: URLString -> PPM (Maybe Path)
handle_url_cache url = do
  urls <- gets alreadies_urls
  let f = (snd<$>) . safe_head . filter ((==url) . fst)
  -- liftIO $ putStrLn $ ">>>>>>>>>>>>>>>>     " ++ show urls
  return $ f urls


