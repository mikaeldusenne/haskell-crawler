module Scrapper where

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
import System.IO.Temp
import Control.Monad.Zip
import System.IO.Error

import Types
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)


mkurl s | isPrefixOf "http" s = return s
        | otherwise = do
            b <- gets base
            return $ b </> applyIf (isPrefixOf "/") tail s


cookiefilepath = "cookies"

login :: [String] -> PPM ()
login [login, pass] = do
  b <- gets base
  lp <- gets login_path
  logk <- gets login_arg_login
  passk <- gets login_arg_pass
  liftIO $ curlPost (b </> lp) [logk++"="++login, passk++"="++pass]

login' = do
  prefix <- gets login_env_prefix
  liftIO (mapM (getEnv . (prefix++)) ["LOGIN", "PASS"]) >>= login

openURL :: URLString -> PPM String
openURL x = do
  x' <- mkurl x
  liftIO $ snd <$> curlGetString x' [CurlFollowLocation True, CurlCookieFile cookiefilepath]

mkname e = map f . trim "\r\n \t?" $ e
  where f x = applyIf (not.(`elem` alpha_num)) (\_ -> '_') x


-- tag_class_f t cl f = map f . filter (~== TagOpen t [("class", cl)])
tag_class_f ::
  String -> String -> (Tag [Char] -> b) -> [Tag [Char]] -> [b]
tag_class_f t cl f = map f . filter ((cl `elem`) . words . fromAttrib "class") . filter (~== TagOpen t [])




url_remove_parameters = reduce (</>)  . takeWhile (not . (isPrefixOf "?")) . splitPath
url_basename = last . takeWhile (not . (isPrefixOf "?")) . splitPath

pathdiff a b = reduce (</>) $ take (length a' - 1) (repeat "..") ++ a'
  where (same, a') = applyToSnd (map fst) . span (\(a,b) -> a==b) . uncurry zip . applyToTuple splitPath $ (a, b)

download :: String -> FilePath -> PPM (Either String ())
download url dest =
  do
    url' <- mkurl url
    shafile <- gets sha256sumsfile
    dldir <- gets download_folder
    shas <- gets alreadies
    cfg <- get
    let shf = dldir </> shafile
    let downloadfile = do
          tmp <- liftIO $ emptyTempFile dldir "tmpdownload"
          let update_alreadies sha = do
                put (cfg{alreadies=append (sha, dest) shas})
                liftIO $ appendFile shf . (++"\n") $ show (sha, dest)
              dltmp = (liftIO) . (BS.writeFile tmp)
              finish (Left e) = do
                liftIO $ removeFile tmp
                return (Left e)
              finish (Right _) = do
                sha <- liftIO $ (show . sha256) <$> LB.readFile tmp
                let already = (snd <$>) . safe_head $ filter ((==sha) . fst) $ shas
                case already of
                  Nothing -> do
                    liftIO (renamePath tmp dest)
                    update_alreadies sha
                    return $ Right ()
                  Just originalpath -> do
                    liftIO $ do
                      removeFile tmp
                      putStrLn $ "Symlink " ++ dest ++ " --> " ++ originalpath
                      createSymbolicLink (pathdiff originalpath dest) dest
                    return $ Right ()
          liftIO $ threadDelay (2*10^5) >>
            putStr (url' ++ "  ")
          filebs <- liftIO $ openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url'
          dl <- case filebs of
            Left e -> return (Left e)
            Right bs -> Right <$> dltmp bs
          finish dl
    -- ex <- liftIO ((liftM (||)) (doesPathExist dest) (pathIsSymbolicLink dest) )
    pathok <- liftIO $ doesPathExist dest
    
    issym <- liftIO $ pathIsSymbolicLink dest `catchIOError` (\_ -> return False)
      
    -- ex <- liftIO $ doesPathExist dest >>= (\pex -> pathIsSymbolicLink dest >>= (\sex -> return (pex || sex)))
      
    if (pathok || issym)
      then (liftIO $ (
      do
        putStrLn $ " skipping, path exists."
        when (issym) (
          do
            putStrLn "bad symlink, fixing..."
            target <- getSymbolicLinkTarget dest
            putStrLn $ "old target is: " ++ target
            let target' = joinPath . tail . splitPath $ target
            putStrLn $ "new target is: " ++ target'
            removeFile dest
            createSymbolicLink target' dest
          )
      )) >> return (Right ())
      else downloadfile

downloader :: ([[Tag String] -> [String]]) -> FilePath -> [FilePath] -> String -> PPM (Either String ())
downloader linkgenerators dldir p url3 = do
  let dest =  reduce (</>) $ dldir:p
  logtag <- gets login_needed_tag
  liftIO $ do
    putStr dest
    createDirectoryIfMissing True $ dest
  
    putStrLn " Download..."
    putStrLn $ nice'title url3
    
  tags <- parseTags <$> openURL url3
  
  let links :: [String]
      links = (filter ((>0) . length)) . flatten . map ($tags) $ linkgenerators
  let process e = download e (dest </> url_basename e)
      
  if (length links > 0)
    then ((reduce (>>))<$>) $ mapM process links
    else do
      if ((>0) . length . filter (~== logtag) $ tags)
        then do
          liftIO $ putStrLn "Re-logging-in..."
          login'
          downloader linkgenerators dldir p url3
        else do
        let errmsg = "error: nothing to download in " ++ url3
        -- liftIO $ print tags >> (putStrLn $ "error: nothing to download in " ++ url3)
        return (Left errmsg)

ff :: T -> [T -> T'] -> PPM [Either (String, String) ()]
ff _ [] = return $ [Right ()]
ff x@(paths, url) (fm:fms) = do
  dld <- gets download_folder
  let finishedpath = dld </> joinPath paths </> "finished"
  alreadyDone <- liftIO $ fileExist finishedpath
  
  if alreadyDone
    then do
    liftIO $ putStrLn $ joinPath paths ++ " has already been downloaded."
    return $ [Right ()]
    else do
      let g k = do
            ans <- fm x
            case ans of
              Left e -> do liftIO $ do
                             putStrLn $ show x ++ "Error, retrying in 2s..."
                             threadDelay (2*10^6)
                           if k>0
                             then g (k-1)
                             else do
                             liftIO (putStrLn (show x ++ " is not working for real...!")) >> return []
                             return $ Left (url, e)
              Right ok -> return $ (Right ok)
                
      next <- g 2
      
      result <- case next of
        Left e -> return $ [Left e]
        Right n -> do
          let n' = map (applyToFst (`append` paths)) n
          (reduce (++)) <$> mapM (`ff` fms) n'

      liftIO $ putStrLn $ "*** Finished <"++ joinPath paths ++"> ***"
      liftIO $ writeFile finishedpath ""
      
      return result
          


-- f :: [T] -> [T -> T'] -> PPM [Either (String, String) ()]
-- f u (fm:fms) = do
--   dld <- gets download_folder
--   liftIO $ print u
--   let run :: [([String], String)] -> PPM [Either (String, String) ()]
--       run [] = return $ [Right ()]
--       run (x@(a,b):xs) = do
--         let finishedpath = dld </> joinPath a </> "finished"
--         alreadyDone <- liftIO $ fileExist finishedpath
--         if alreadyDone
--           then return $ [Right ()]
--           else do
--             let g k = do
--                   ans <- fm x
--                   case ans of
--                     Left e -> do liftIO $ do
--                                    putStrLn $ show x ++ "Error, retrying in 2s..."
--                                    threadDelay (2*10^6)
--                                  if k>0
--                                    then g (k-1)
--                                    else do
--                                    liftIO (putStrLn (show x ++ " is not working for real...!")) >> return []
--                                    return $ Left (b, e)
--                     Right ok -> do
--                       liftIO $ putStrLn $ "*** Finished <"++ joinPath a ++"> ***"
--                       liftIO $ writeFile finishedpath ""
--                       return $ (Right ok)
--             next <- g 2
            
--             ans <- case next of
--               Left e -> return $ [Left e]
--               Right n -> f (map (applyToFst (`append` a)) n) fms
--             (((ans++)<$>)<$>) run xs
--             -- ((ans++)<$>) $ run xs
--   run u
-- f u [] = return $ [Right ()]



extracturl url f = liftIO (threadDelay (10^5)) >> (f . parseTags) <$> openURL url
a_class_href cl = tag_class_f "a" cl (fromAttrib "href")
a_href_f f cl (_, b) = extracturl b (\tags -> let
                                        hr = (a_class_href cl tags)
                                        in zip (map f hr) hr)
a_href_basename = a_href_f basename
