{-# LANGUAGE LambdaCase #-}
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
-- import Control.Monad.Trans.Either

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

download :: URLString -> FilePath -> PPM (Either String ())
download url dest =
  do
    url' <- mkurl url
    -- liftIO $ putStrLn $ "Downloading " ++ url' ++ " to " ++ dest ++ "..."
    dldir <- gets download_folder
    cfg <- get
    tmp <- liftIO $ emptyTempFile dldir "tmpdownload"

    liftIO $ do
      threadDelay (2*10^5) >>
        putStrLn url'
      
      (openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url') >>= (
        \case
          Right bs -> do
            BS.writeFile tmp bs
            -- putStrLn $ "renaming " ++ surround "'" tmp ++ " to " ++ surround "'" dest
            renamePath tmp dest
            return $ Right ()
          Left e -> return $ Left e
        )
    
downloader :: ([[Tag String] -> [String]]) -> FilePath -> [FilePath] -> String -> PPM (Either String ())
downloader linkgenerators dldir p url = do
  let dest = reduce (</>) $ dldir:p
  logtag <- gets login_needed_tag
  liftIO $ do
    putStr dest
    createDirectoryIfMissing True $ dest
  
    putStrLn " Download..."
    putStrLn $ nice'title url
    
  tags <- parseTags <$> openURL url
  
  let links :: [String]
      links = (filter ((>0) . length)) . flatten . map ($tags) $ linkgenerators
  let process e = do
        let destpath = dest </> fix (url_basename e)
              where fix x | isSuffixOf "/" x = init x ++ ".html"
                          | otherwise = x

        -- liftIO $ putStrLn $ "{process} " ++ e ++ " ==@ " ++ destpath

        pathok <- liftIO $ doesPathExist destpath
    
        issym <- liftIO $ pathIsSymbolicLink destpath `catchIOError` (\_ -> return False)
    
        let fix_symlink = do
              liftIO$ putStrLn "bad symlink, fixing..."
              target <- liftIO$ getSymbolicLinkTarget destpath
              liftIO$ putStrLn $ "old target is: " ++ target
              let target' = joinPath . tail . splitPath $ target
              liftIO$ removeFile destpath
              
              ifm (liftIO$ doesPathExist target')
                (do
                    liftIO$do
                      putStrLn $ "new target is: " ++ target'
                      createSymbolicLink target' destpath
                    return (Right ())
                )
                (do
                    liftIO$ putStrLn $ target' ++ " does not exist either. deleting symlink"
                    download url destpath
                )
        let download_or_cache = do
              handle_url_cache e >>= (
                \case 
                  Nothing -> do
                    
                    download e destpath
                    
                    cfg <- get
                    urls <- gets alreadies_urls
                    -- liftIO . putStrLn $ "adding " ++ e ++ " to the FUCKING list"
                    put (cfg{ alreadies_urls = append (e, destpath) urls})
                    liftIO . (`appendFile` (show (e, destpath) ++ "\n")) $ download_folder cfg </> urlsfile cfg
                    return $ Right ()
                  Just originalpath -> do
                    liftIO $ do
                      -- print $ "wtf WTH" ++ originalpath
                      putStrLn $ "Symlink " ++ destpath ++ " --> " ++ originalpath
                      createSymbolicLink (pathdiff originalpath destpath) destpath
                    return $ Right ()
                )
              
        
        if (pathok || issym)
          then do
          liftIO$ putStrLn $ " skipping, path exists."
          if (issym && not pathok)
            then fix_symlink
            else return (Right ())
          else download_or_cache
  
      
  if (length links > 0)
    then do
      (((reduce (>>))<$>) $ mapM process links) >> process url
    else do
      if ((>0) . length . filter (~== logtag) $ tags)
        then do
          liftIO $ putStrLn "Re-logging-in..."
          login'
          downloader linkgenerators dldir p url
        else do
        let errmsg = "error: nothing to download in " ++ url
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
          ans <- (reduce (++)) <$> mapM (`ff` fms) n'
          liftIO $ putStrLn $ "*** Finished <"++ joinPath paths ++"> ***"
          liftIO $ writeFile finishedpath ""
          return ans
      
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
