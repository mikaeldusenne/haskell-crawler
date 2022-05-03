module Helpers where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

import Data.List (isPrefixOf)
import Control.Monad.Trans.State (gets)
import qualified Network.Curl as CURL

import Types (PPM, base)
import Misc (joinPaths, applyIf, alpha_num)
import List (splitOn, trim)


-- clean string to make it fs filename friendly
mkname :: String -> String
mkname = map f . trim "\r\n \t?"
  where f x = applyIf (not.(`elem` alpha_num)) (\_ -> '_') x


-- mkurl: appends base url from config if relative path is provided
mkurl :: String -> PPM String
mkurl s = if isPrefixOf "http" s
  then return s
  else (`joinPaths`s) <$> gets base


-- opens a url with optional cookies management
openURL :: Maybe FilePath -> CURL.URLString -> PPM (Either String String)
openURL cooks x = do
  x' <- mkurl x
  let opts = [CURL.CurlFollowLocation True] ++ (case cooks of Just cookiefilepath -> [CURL.CurlCookieFile cookiefilepath]
                                                              Nothing -> [])
  liftIO $ (Right . snd) <$> CURL.curlGetString x' opts
  

-- retry f at most `k` times
-- retry :: Monad m => m (Either a b) -> Int -> m (Either a b)
type NamedError a = (String, a)
type NamedErrorStr = NamedError String
retry :: Int -> String -> PPM (Either String b) -> PPM (Either NamedErrorStr b)
retry k description f = do
  ans <- f
  case ans of
    Left e -> do
      liftIO $ putStrLn $ description ++ "Error, retrying in 2s..."
      liftIO $ threadDelay (2*10^6)
      if k > 0
        then retry (k-1) description f
        else do
        -- liftIO (putStrLn (description ++ " is not working for real...!"))
        return $ Left (description, e)
    Right ok -> return $ (Right ok)


