module Hafez where

import qualified Data.Text as T (pack, replace, unpack)
import Text.HTML.TagSoup
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix ((</>), joinPath, splitPath)
import Control.Monad.Trans.State (gets, put, get, StateT, runStateT)
import List (splitOn, basename, dirname)

import Data.List (isSuffixOf)
import System.Directory

import Types
import Scrapper
import Misc (joinPaths)

-----------------------------------------------------------


concaturl a b = joinPaths (dirname a) (b)
  
cleanUrl u = if isSuffixOf "index.htm" u then take (length u - length "index.htm") u else u

-- hafizonlove first and second level
-- findlinks :: [Tag String] -> [String]
findlinks :: UrlWithDest -> PPM (Either String [UrlWithDest])
findlinks UrlWithDest{url=url} = do
  -- liftIO $ print ("LOADING:", urldldir</>)
  extracted <- f <$> extracturl url
  -- liftIO $ print ("FOUND:", extracted)
  return extracted
  where f = Right . map curateHref . filter (~=="<a>") . takeWhile (~/= "<p>") . dropWhile (~/= "<li>") . dropWhile (~/= "<div class=linklist>")
        curateHref e =  UrlWithDest (concaturl url hr) (basename $ cleanUrl hr)
          where hr = fromAttrib "href" e
-- hafizonlove content download


curatePoem = T.unpack
  . T.replace (T.pack "\n\r\n") (T.pack "\n")
  -- . T.replace (T.pack "") (T.pack "\n")
  . T.pack

savePoem :: FilePath -> [String] -> PPM ()
savePoem dest [en, fa] = do
  liftIO $ do
    createDirectoryIfMissing True dest
    writeFile (dest</>"en.txt") $ curatePoem en
    writeFile (dest</>"fa.txt") $ curatePoem fa
savePoem dest x = error $ show x

    
replaceTags :: (Tag String -> Bool) -> Tag String -> [Tag String] -> [Tag String]
replaceTags pred r l = map f l
  where f e | pred e = r
            | otherwise = e
            

-- genPoem :: FilePath -> [Tag String] -> PPM ()
-- genPoem dest = savePoem dest .
--   map run . splitOn (\e -> e ~== "<Div>" && (fromAttrib "id" e `elem` ["Eng", "farsi"])) .
--   drop 2 . dropWhile (~/= "<Div id=Main>")
--   where
--     run :: [Tag String] -> String
--     run = unlines . map (unwords . words . fromTagText) . takeWhile isText . filter (~/="<br>") . drop 1 . dropWhile (~/= "<p>")
--     isText (TagText _) = True
--     isText _ = False

genPoem :: UrlWithDest -> PPM (Either String [UrlWithDest])
genPoem UrlWithDest{url=url, dest=dest} = do
  let f = map run . splitOn (\e -> e ~== "<Div>" && (fromAttrib "id" e `elem` ["Eng", "farsi"])) .
        drop 2 . dropWhile (~/= "<Div id=Main>")
  urlextracted <- extracturl url
  let poems = f urlextracted
  -- liftIO $ print (url, urlextracted, poems)
  liftIO $ writeFile (dest</>"all.txt") $ "--------------------" ++ show urlextracted ++ "\n====\n" ++ show poems

  liftIO $ putStrLn url
  savePoem dest poems
  -- (f <$> extracturl url) >>= savePoem dest
  return $ Right []
  where
    run :: [Tag String] -> String
    run = unlines . map (unwords . words . fromTagText) . takeWhile isText . filter (~/= "</br>") . filter (~/="<br>") . filter (~/= "</p>") . filter (~/="<p>") . drop 1 . dropWhile (~/= "<p>")
    isText (TagText _) = True
    isText _ = False



-- genPoem = -- map run
--           -- . filter (\(x:xs) -> fromAttrib "id" x `elem` ["Eng", "farsi"])
--           splitOn (~=="<div>") . dropWhile (~/= "<div id=Eng>") . dropWhile (~/= "<div id=Main>")




-----------------------------------------------------------


mkcfg :: IO Config
mkcfg = createConfig $ defaultconfig {
        download_folder = "./hafez_downloads/",
        base = "http://www.hafizonlove.com/divan/"
        }

fs :: [UrlWithDest -> PPM (Either String [UrlWithDest])]
fs = [
  findlinks,
  findlinks,
  genPoem
     ]

