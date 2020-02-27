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

type Pathl = [String]
type T = (Pathl, String)
type T' = IO [(String, String)]

base = "https://www.persianpod101.com/"

mkurl s = base </> applyIf ((== '/').head) tail s

l = reverse $ words "absolute-beginner beginner intermediate advanced bonus"

cookiefilepath = "cookies"

login [login, pass] = curlPost "https://www.persianpod101.com/member/login_new.php" ["amember_login="++login,"amember_pass="++pass]

login' = mapM (getEnv . ("pp101_"++)) ["LOGIN", "PASS"] >>= login

openURL x = snd <$> curlGetString x [CurlFollowLocation True, CurlCookieFile cookiefilepath]

isalreadythere dest = (any (== basename dest) . map dropExtension ) <$> listDirectory (dirname dest)

download url dest = threadDelay (2*10^5) >> openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url
                    >>= dltmp
  where dltmp (Right bs) = BS.writeFile tmppath bs >> renamePath tmppath dest
        dltmp (Left err) = putStrLn err
        tmppath = "tmp.persianpod101.download"

extracturl url f = threadDelay (10^5) >> (f . parseTags) <$> openURL url

trimm = reverse . f . reverse . f
  where f = dropWhile (`elem` "\r\n \t?")

mkname e = map f . trimm $ e
  where f x = applyIf (not.(`elem` alpha_num)) (\_ -> '_') x

href = (mkurl . fromAttrib "href")

-- tag_class_f t cl f = map f . filter (~== TagOpen t [("class", cl)])
tag_class_f t cl f = map f . filter ((cl `elem`) . words . fromAttrib "class") . filter (~== TagOpen t [])

a_class_href cl = tag_class_f "a" cl href

textIn tag match = map (fromTagText  . head) . tail . splitOn  (~== TagOpen tag match)
extract_lesson_name = textIn "div" [("class", "cl-lesson__title")]

goo dldir p url3 = do
  let dest =  reduce (</>) $ dldir:p
  putStr dest
  createDirectoryIfMissing True $ dirname dest
  
  isalreadythere dest >>=
    ((putStrLn $ " skipping, file exists.") !∫
     (do
         print "Download..."
         putStrLn $ nice'title url3
         url4 <- parseTags <$> openURL url3
         let fs = map (safe_head.) [
               tag_class_f "button" "js-lsn3-play-lesson-audio" (fromAttrib "data-url"),
               tag_class_f "video" "js-lsn3-main-video-player" (fromAttrib "data-trackurl")
               ]
             process Nothing = return "no"
             process (Just e) = download e (dest++takeExtension e) >> return "ok"
         ans <- mapM (process . ($url4) ) fs
         when (all (=="no") ans) (
           do
             print url4
             putStrLn $ "error with " ++ url3
             when ((>0) $ length . filter (~== TagOpen "button" [("aria-label","Please sign in")]) $ url4) (
               do
                 putStrLn "Re-logging-in..."
                 login'
               )
           )
     ))

f u (fm:fms) = do
  (`mapM_` u) (\(a, b) -> do
                  -- print a
                  ans <- fm (a, b)
                  f (map (applyToFst (($a) . append)) ans) fms
              )
f u [] = return ()


a_href_f f cl (_, b) =  b `extracturl` (\tags -> let hr = (a_class_href cl tags)
                                                 in zip (map f hr) hr)

a_href_basename = do
  a_href_f basename
prepare_lesson = \(a,b) -> b `extracturl` (\tags -> zip
                                             (zipWith (\ a b -> a++"_"++b) (map (fill'left 3 '0'.show) [1..]) (map mkname $ extract_lesson_name tags))
                                     (a_class_href "cl-lesson__lesson" tags))

main = do
  login'
  download_dest <- (\l -> if length l > 0 then head l else "downloads") <$> getArgs
  let dlf = (>> return []) . uncurry (goo download_dest) 
  
  let commonfs = [\ (a,b) -> return $ [("", mkurl $ ("lesson-library"</>) b)] ]
      fs = [
        [a_href_basename "ll-collection-all"],
        [a_href_basename "ll-recommended-collection__study"]
        ]
        
  mapM_ (f (zip (map (:[]) l) l) . (++ [prepare_lesson, dlf]) . (commonfs++)) $ fs
  putStrLn "ok"
