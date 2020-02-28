module Types where

import Control.Monad.Trans.State
import Text.HTML.TagSoup
import Data.Digest.Pure.SHA
import System.FilePath.Posix

type Pathl = [String]
type T = (Pathl, String)
type T' = PPM (Either String [(String, String)])


type PPM a = StateT Config IO a

type Cache = [(String, String)]

type SHAString = String

data Config = Config {
  download_folder :: String,
  base :: String,
  login_path :: String,
  login_arg_login :: String,
  login_arg_pass :: String,
  sha256sumsfile :: String,
  login_needed_tag :: Tag String,
  login_env_prefix :: String,
  alreadies :: [(SHAString, String)]
  }
  deriving (Show)

defaultconfig = Config {
  download_folder = "scrapper_downloads",
  base = "https://hello-world.com/",
  login_path = "login.php",
  login_arg_login = "member_login",
  login_arg_pass = "member_pass",
  sha256sumsfile = "sha256sums.txt",
  login_needed_tag = TagOpen "button" [("aria-label","Please sign in")],
  login_env_prefix = "scrapper_",
  alreadies = []
  }

createConfig cfg = do
  shas <- map read . lines <$> readFile (download_folder cfg </> sha256sumsfile cfg)
  return cfg{alreadies=shas}
  
  
  
