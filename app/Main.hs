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

type Pathl = [String]
type T = (Pathl, String)
type T' = IO [(String, String)]

base = "https://www.persianpod101.com/"

mkurl s | isInfixOf "http" s = s
        | otherwise = base </> applyIf ((== '/').head) tail s

cookiefilepath = "cookies"

login [login, pass] = curlPost "https://www.persianpod101.com/member/login_new.php" ["amember_login="++login,"amember_pass="++pass]

login' prefix = mapM (getEnv . (prefix++)) ["LOGIN", "PASS"] >>= login

openURL x = snd <$> curlGetString x [CurlFollowLocation True, CurlCookieFile cookiefilepath]


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

sha256sumsfile = "sha256sums.txt"

url_remove_parameters = reduce (</>)  . takeWhile (not . (isPrefixOf "?")) . splitPath
url_basename = last . takeWhile (not . (isPrefixOf "?")) . splitPath


download dldir url dest = doesFileExist dest >>= (
  (putStrLn $ " skipping, file exists.") !∫
     (do threadDelay (2*10^5) >> putStr (url ++ "  ") >> 
           openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url
           >>= dltmp >> finish
     ))
  where dltmp (Right bs) = BS.writeFile tmppath bs
        dltmp (Left err) = putStrLn err
        finish = do
          let shf = dldir </> sha256sumsfile
          sha <- (show . sha256) <$> LB.readFile tmppath
          shas <- map read . lines <$> (readFile shf)
          let already = (snd <$>) . safe_head $ filter ((==sha) . fst) $ shas
          
          case already of
            Nothing -> do
              putStrLn "New file"
              renamePath tmppath dest
              writeFile shf . unlines . map show $ (sha, dest) : shas
            Just originalpath -> do
              putStrLn $ "Symlink " ++ dest ++ " --> " ++ originalpath
              createSymbolicLink originalpath dest
          
        tmppath = "tmp.persianpod101.download"


downloader :: ([[Tag String] -> [String]]) -> FilePath -> [FilePath] -> String -> IO ()
downloader linkgenerators dldir p url3 = do
  let dest =  reduce (</>) $ dldir:p
      
  putStr dest
  createDirectoryIfMissing True $ dest
  
  putStrLn " Download..."
  putStrLn $ nice'title url3
  tags <- parseTags <$> openURL url3
  let links :: [String]
      links = (filter ((>0) . length)) . flatten . map ($tags) $ linkgenerators
  let process e = download dldir e (dest </> url_basename e)
   
  if (length links > 0)
    then mapM_ process links
    else do
      if ((>0) $ length . filter (~== TagOpen "button" [("aria-label","Please sign in")]) $ tags)
        then do
          putStrLn "Re-logging-in..."
          login_pp101
          downloader linkgenerators dldir p url3
        else print tags >> (putStrLn $ "error: nothing to download in " ++ url3)

f :: [T] -> [T -> T'] -> IO ()
f u (fm:fms) = do
  print u
  (`mapM_` u) (\(a, b) -> fm (a, b) >>=
                \ans -> f (map (applyToFst (`append` a)) ans) fms
              )
f u [] = return ()


extracturl url f = threadDelay (10^5) >> (f . parseTags) <$> openURL url


a_href_f f cl (_, b) =  extracturl b (\tags -> let hr = (a_class_href cl tags)
                                                 in zip (map f hr) hr)

a_href_basename = a_href_f basename


--------------------------------------------------------------------


prepare_lesson = \(a,b) -> extracturl b
  (\tags -> zip
            (zipWith (\ a b -> a++"_"++b) (map (fill'left 3 '0'.show) [1..])
              (map mkname $ extract_lesson_name tags))
            (a_class_href "cl-lesson__lesson" tags)
  )

login_pp101 = login' "pp101_"

main = do
  login_pp101
  download_dest <- (\l -> if length l > 0 then head l else "downloads") <$> getArgs
  let shf = (download_dest </> sha256sumsfile) in
    (not <$> doesFileExist shf) >>= (`when` writeFile shf "")
  
  let l = words "absolute-beginner beginner intermediate advanced bonus"
  
  let start :: [T]
      start = map (\e -> ([e], mkurl $ "lesson-library" </> e)) l
      fs :: [T -> T']
      fs = [
        \e -> do
          a <- a_href_basename "ll-collection-all" e
          b <- a_href_basename "ll-recommended-collection__study" e
          return $ a ++ b,
        prepare_lesson,
        dlf
        ]
      dlf = (>> return []) . uncurry (downloader linksg download_dest)
        where linksg = [
                tag_class_f "button" "js-lsn3-play-lesson-audio" (fromAttrib "data-url"),
                tag_class_f "video" "js-lsn3-main-video-player" (fromAttrib "data-trackurl"),
                map mkurl . uniq . filter (isPrefixOf  "/pdf") . map (fromAttrib "href") . filter (~== TagOpen "a" [])
                ]
  
  f start fs
  putStrLn "ok"
