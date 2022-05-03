{-# LANGUAGE LambdaCase #-}
module Scrapper where

import qualified Data.ByteString as BS
import List

import Data.List (isPrefixOf, isSuffixOf)
import Data.Char(toLower)
import Network.Curl
import Network.Curl.Download
import Text.HTML.TagSoup
import qualified Data.Text as T (pack, replace, unpack)
import System.Directory
import System.IO
import System.FilePath.Posix ((</>), joinPath, splitPath)

import Misc (applyIf, alpha_num, ifm, joinPaths)
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

import List (splitOn)
import Types
import Control.Monad.Trans.State (gets, put, get, StateT, runStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Helpers (retry, openURL, mkurl)
import qualified Secrets

cookiefilepath = "cookies"

login :: [String] -> PPM ()
login [login, pass] = do
  b <- gets base
  Just lp <- gets login_path
  logk <- gets login_arg_login
  passk <- gets login_arg_pass
  liftIO $ curlPost (b </> lp) [logk++"="++login, passk++"="++pass]

login' = login [Secrets.login, Secrets.pass] -- do
  -- prefix <- gets login_env_prefix
  -- liftIO (mapM (getEnv . (prefix++)) ["LOGIN", "PASS"]) >>= login
 

-- tag_class_f t cl f = map f . filter (~== TagOpen t [("class", cl)])
-- apply f to all tags `t` with class `cl`
tag_class_f ::
  String -> String -> (Tag [Char] -> b) -> [Tag [Char]] -> [b]
tag_class_f t cl f = map f . filter ((cl `elem`) . words . fromAttrib "class") . filter (~== TagOpen t [])




url_remove_parameters = reduce (</>)  . takeWhile (not . (isPrefixOf "?")) . splitPath
url_basename = last . takeWhile (not . (isPrefixOf "?")) . splitPath

pathdiff a b = reduce (</>) $ take (length a' - 1) (repeat "..") ++ a'
  where (same, a') = applyToSnd (map fst) . span (\(a,b) -> a==b) . uncurry zip . applyToTuple splitPath $ (a, b)

download :: UrlWithDest -> PPM (Either String ())
download UrlWithDest{url=url, dest=dest} =
  do
    url' <- mkurl url
    dldir <- gets download_folder
    cfg <- get
    tmp <- liftIO $ emptyTempFile dldir "tmpdownload"

    liftIO $ do
      putStrLn url'
      threadDelay (2*10^5)
      (openURIWithOpts [CurlFollowLocation True, CurlCookieFile cookiefilepath] url') >>= (
        \case
          Right bs -> (Right<$>) (BS.writeFile tmp bs >> renamePath tmp dest)
          Left e -> doesPathExist tmp >>= (`when` removeFile tmp) >> (return $ Left e)
        )
    
downloader :: ([[Tag String] -> [URLString]]) -> FilePath -> UrlWithDest -> PPM (Either String ())
downloader linkgenerators dldir p@UrlWithDest{url=url, dest=dest'} = do
  -- let dest = reduce (</>) $ dldir:p
  let dest = dldir </> dest'
  logtag <- gets login_needed_tag
  liftIO $ do
    putStr dest
    createDirectoryIfMissing True $ dest
  
    putStrLn " Download..."
    putStrLn $ nice'title url
    
  -- tags <- parseTags <$> openURL (Just cookiefilepath) url
  tags <- extracturl url
  
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
                    download UrlWithDest{url=url, dest=destpath}
                )
        let download_or_cache :: StateT Config IO (Either String ())
            download_or_cache = do
              handle_url_cache e >>= (
                \case 
                  Nothing -> download UrlWithDest{url=e, dest=destpath} >> do
                    cfg <- get
                    urls <- gets alreadies_urls
                    -- liftIO . putStrLn $ "adding " ++ e ++ " to the FUCKING list"
                    put (cfg{ alreadies_urls = append (e, destpath) urls})
                    liftIO . (`appendFile` (show (e, destpath) ++ "\n")) $ download_folder cfg </> urlsfile cfg
                    return $ Right ()
                  Just originalpath -> do
                    if destpath == originalpath
                      then (do
                      liftIO $ putStrLn $ "warning: original and dest are the same. fixing."
                      cfg <- get
                      put $ cfg{ alreadies_urls=filter (not . (==destpath) . snd) $ alreadies_urls cfg }
                      download_or_cache)
                      else do
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
    then (((reduce (>>))<$>) $ mapM process links) >> process url
    else do
      if ((>0) . length . filter (~== logtag) $ tags)
        then do
          liftIO $ putStrLn "Re-logging-in..."
          login'
          downloader linkgenerators dldir p
        else do
        let errmsg = "error: nothing to download in " ++ url
        -- liftIO $ print tags >> (putStrLn $ "error: nothing to download in " ++ url3)
        return (Left errmsg)




ff :: UrlWithDest -> [UrlWithDest -> PPM (Either String [UrlWithDest])] -> PPM [Either (String, String) ()]
ff _ [] = return $ [Right ()]
ff x@UrlWithDest{dest=path, url=url} (fm:fms) = do
  let finishedpath = path </> "finished"
  
  alreadyDone <- liftIO $ fileExist finishedpath
  if alreadyDone
    
    then do
    liftIO $ putStrLn $ path ++ " has already been downloaded."
    return $ [Right ()]
    
    else do
      next <- retry 2 (show x) (fm x)
      
      let prependPath UrlWithDest{url=url, dest=dest} = UrlWithDest{url=url, dest=path</>dest}
      
      case next of
        Left e -> return $ [Left e]
        Right n -> do
          let n' :: [UrlWithDest]
              n' = map prependPath n
          -- let n' = applyToFst (`append` paths) n
          ans <- flatten <$> mapM (`ff` fms) n'
          -- liftIO $ putStrLn $ "*** Finished <"++ path ++"> ***"
          liftIO $ writeFile finishedpath ""
          return ans
      

-- takes a `url`, curl it and parse tags
extracturl :: URLString -> PPM [Tag String]
-- extracturl u = liftIO (threadDelay (10^5)) >> parseTags <$> openURL (Just cookiefilepath) u
extracturl u = do
  liftIO (threadDelay (10^5))
  Right result <- (retry 3 ("openurl " ++ u) $ openURL (Just cookiefilepath) u)
  return $ parseTags result


-- findlinks = takeWhile (~== "<li>") . dropWhile (~/= "<li>") . dropWhile (~/= "<div class=linklist>")

pretty :: Show a => [a] -> String
pretty l = unlines . map show $ l


mainLoop :: Config -> [UrlWithDest -> PPM (Either String [UrlWithDest])] -> IO ()
mainLoop cfg fs = do
  let isNothing Nothing = True
      isNothing _ = False
      
      -- run :: PPM [Either String ()]
      run = do
        login_path' <- gets login_path
        when (not . isNothing $ login_path') login'

        -- let -- start :: [T]
            -- start = map (\e -> UrlWithDest{url=e, dest="lesson-library" </> e}) l
            -- fs :: [T -> T']
        -- reduce (++) <$> mapM (`ff` fs) [defaultUrlWithDest]
        dld <- gets download_folder
        ff defaultUrlWithDest{dest=dld} fs 

        
  status <- runStateT run cfg
  putStrLn "---------------------------------"
  putStrLn "errors:"
  (`mapM` fst status) (\e -> case e of
                         Right () -> return ()
                         Left err -> do
                           -- putStrLn $ boxify (fst err) [snd err]
                           putStrLn $ show ((fst err), [snd err])
                      )
  putStrLn "ok"
