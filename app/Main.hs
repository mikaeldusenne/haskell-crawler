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


base = "https://www.persianpod101.com/"

mkurl s = base </> applyIf ((== '/').head) tail s

l = words "absolute-beginner beginner intermediate advanced bonus"

cookiefilepath = "cookies"

login [login, pass] = curlPost "https://www.persianpod101.com/member/login_new.php" ["amember_login="++login,"amember_pass="++pass]

openURL x = snd <$> curlGetString x [CurlFollowLocation True, CurlCookieFile cookiefilepath]

isalreadythere dest = (any (== basename dest) . map dropExtension ) <$> listDirectory (dirname dest)

download url dest = openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url
                    >>= dltmp
  where dltmp (Right bs) = BS.writeFile tmppath bs >> renamePath tmppath dest
        dltmp (Left err) = print err
        tmppath = "tmp.persianpod101.download"

extracturl url f = (f . parseTags) <$> openURL url

trimm = reverse . f . reverse . f
  where f = dropWhile (`elem` "\r\n \t?")

mkname e = map f . trimm $ e
  where f x = applyIf (not.(`elem` alpha_num)) (\_ -> '_') x

-- mkdest l = foldl (</>) "" . (l++) . (:[])

hreff = (mkurl . fromAttrib "href")

tag_class_f t cl f = map f . filter (~== TagOpen t [("class", cl)])
a_class_href cl = tag_class_f "a" cl hreff

-- goo download_dest section (k, (name, url3)) = do
--   let dest = mkdest [download_dest, section] $ mkname k name
--   createDirectoryIfMissing True $ dirname dest
  
--   isalreadythere dest >>=
--     ((putStrLn $ "skipping '"++dest++"', file exists.") !∫
--      (do
--          putStrLn $ "Dl " ++ url3 ++ "..."
--          url4 <- parseTags <$> openURL url3
--          let fs = map (safe_head.) [
--                tag_class_f "button" "js-lsn3-play-lesson-audio" (fromAttrib "data-url"),
--                tag_class_f "video" "js-lsn3-main-video-player" (fromAttrib "data-trackurl")
--                ]
--              process Nothing = return "no"
--              process (Just e) = download e (dest++takeExtension e) >> return "ok"
--          putStrLn $ show k ++ " --> " ++ dest
--          ans <- mapM (process . ($url4) ) fs
--          when (all (=="no") ans) (putStrLn $ "error with " ++ url3)
--      ))
    
goo dldir p url3 = do
  let dest =  reduce (</>) $ dldir:p
  createDirectoryIfMissing True $ dirname dest
  
  isalreadythere dest >>=
    ((putStrLn $ "skipping '"++dest++"', file exists.") !∫
     (do
         putStrLn $ "Dl " ++ url3 ++ "..."
         url4 <- parseTags <$> openURL url3
         let fs = map (safe_head.) [
               tag_class_f "button" "js-lsn3-play-lesson-audio" (fromAttrib "data-url"),
               tag_class_f "video" "js-lsn3-main-video-player" (fromAttrib "data-trackurl")
               ]
             process Nothing = return "no"
             process (Just e) = download e (dest++takeExtension e) >> return "ok"
         putStrLn dest
         ans <- mapM (process . ($url4) ) fs
         when (all (=="no") ans) (putStrLn $ "error with " ++ url3)
     ))

extract_lesson_name = map (fromTagText  . head) . tail . splitOn  (~== TagOpen "div" [("class", "cl-lesson__title")])

f :: [T] -> [T -> T'] -> IO ()
f u (fm:fms) = do
  (`mapM_` u) (\(a, b) -> do
            print a
            ans <- fm (a, b)
            f (map (applyToFst (($a) . append)) ans) fms
        )
f u [] = return ()

type Pathl = [String]
type T = (Pathl, String)
type T' = IO [(String, String)]


main = do
  mapM (getEnv . ("pp101_"++)) ["LOGIN", "PASS"] >>= login
  download_dest <- (\l -> if length l > 0 then head l else "downloads") <$> getArgs
  let dlf = (>> return []) . uncurry (goo download_dest) 
  
  let commonfs = [\ (a,b) -> return $ [(head a, mkurl $ ("lesson-library"</>) b)] ] :: [T -> T']
      -- fs :: [URLString -> IO [FilePath]]
      fs :: [[T -> T']]
      fs = [
        [
          \(a,b) -> b `extracturl` (\tags -> let hr = (a_class_href "ll-recommended-collection__study" tags)
                                             in zip
                                                (map basename hr)
                                                hr),
          \(a,b) -> b `extracturl` (\tags -> zip
                                             (zipWith (\ a b -> a++"_"++b) (map (fill'left 3 '0'.show) [1..]) (map mkname $ extract_lesson_name tags))
                                     (a_class_href "cl-lesson__lesson" tags)),
          dlf
        ]
        ]
  -- f (map (applyToFst ((:[]) . show)) $ enumerate l) $ commonfs ++ fs
  mapM_ (f (zip (repeat [""]) l) . (commonfs++)) $ fs

  -- let go l = do
  
  --       putStrLn $ nice'title l
  --       let url_section = mkurl $ "lesson-library/" </> l
  --       url_recommended <- extracturl url_section (a_class_href "ll-recommended-collection__study")
  --       -- case url_recommended of
  --       --   Nothing -> putStrLn $ "there is nothing to download from " ++ url_section
  --       --   Just u ->
        
  --       (`mapM_` url_recommended) $ \u -> do
  --           urls <- extracturl u (\tags -> zip (extract_lesson_name tags) (a_class_href "cl-lesson__lesson" tags))
  --           print $ u ++ " ; " ++ show (length urls) ++ " urls found."        
  --           mapM_ (goo download_dest l) (zip [1..] urls)

  -- mapM go l
  putStrLn "ok"
