module Main where

import qualified Data.ByteString as BS
import Lib
import List

import Data.List
import Data.Char(toLower)
import Network.Curl
import Network.Curl.Download
import Text.HTML.TagSoup
import System.Directory
import System.IO
import System.FilePath.Posix
import Misc
import Tuple
import System.Environment (getEnv, getArgs)
import Control.Monad (when, foldM_)
import Control.Concurrent (threadDelay)
import System.Posix.Files
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as LB
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)


import Types
import Scrapper

textIn tag match = (map (fromTagText  . head)<$>) . safe_tail . splitOn  (~== TagOpen tag match)
  

extract_lesson_name = textIn "div" [("class", "cl-lesson__title")]

prepare_lesson :: (a, URLString) -> T'
prepare_lesson (a,b) = extracturl b f
  where f tags = ((`zip` url)<$>) name 
          where name = (zipWith (\ a b -> a++"_"++b) (map (fill'left 3 '0'.show) [1..]) <$>) names
                names = case ((map mkname <$>) $ extract_lesson_name tags) of
                  Nothing -> Left ("lesson name extraction failed for " ++ b) -- putStrLn $ 
                        -- "wtf " <> show tags <> "\n" <> b <> " \nwtf"
                  Just e -> Right e
                url = (a_class_href "cl-lesson__lesson" tags)
  


main = do
  download_dest <- (\l -> if length l > 0 then head l else "downloads") <$> getArgs

  cfg <- createConfig $ defaultconfig {
          download_folder = download_dest,
          base = "https://www.persianpod101.com/",
          login_path = "member/login_new.php",
          login_arg_login = "amember_login",
          login_arg_pass = "amember_pass",
          sha256sumsfile = "sha256sums.txt",
          login_needed_tag = TagOpen "button" [("aria-label","Please sign in")],
          login_env_prefix = "pp101_"
          }

  let -- run :: PPM [Either String ()]
      run = do
        login'
        ((download_dest </>) <$> gets sha256sumsfile) >>= \shf ->
          liftIO $ (not <$> doesFileExist shf) >>= (`when` writeFile shf "")
        let l = words "absolute-beginner beginner intermediate advanced bonus"
        let start :: [T]
            start = map (\e -> ([e], "lesson-library" </> e)) l
            fs :: [T -> T']
            fs = [
              \e -> do
                a <- a_href_basename "ll-collection-all" e
                b <- a_href_basename "ll-recommended-collection__study" e
                return . Right $ a ++ b,
              prepare_lesson,
              dlf
              ]
            dlf = (>> return (Right [])) . uncurry (downloader linksg download_dest)
              where linksg = [
                      tag_class_f "button" "js-lsn3-play-lesson-audio" (fromAttrib "data-url"),
                      tag_class_f "video" "js-lsn3-main-video-player" (fromAttrib "data-trackurl"),
                      uniq . filter (isPrefixOf  "/pdf") . map (fromAttrib "href") . filter (~== TagOpen "a" [])
                      ]
        reduce (++) <$> mapM (`ff` fs) start

        
  status <- runStateT run cfg
  putStrLn "---------------------------------"
  putStrLn "errors:"
  (`mapM` fst status) (\e -> case e of
                         Right () -> return ()
                         Left err -> do
                           putStrLn $ boxify (fst err) [snd err]
                      )
  putStrLn "ok"

