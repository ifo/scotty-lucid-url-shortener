{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Lucid

import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import qualified Data.Map       as M
import qualified Data.Text.Lazy as LT
import System.Random (randomRs, newStdGen, RandomGen)

main :: IO ()
main = scotty 3000 $ do

  urlMap <- liftIO $ newMVar (M.empty :: M.Map LT.Text LT.Text)

  get "/" $
    html . renderText $
      html_ $
        body_ $ do
          h1_ "Title"
          p_ "Hello Lucid World!"
          with form_ [method_ "post", action_ "/"] $ do
            input_ [type_ "text", name_ "url"]
            with button_ [type_ "submit"] "Shorten"

  post "/" $ do
    url <- param "url"
    gen <- liftIO newStdGen
    let shortenedUrl = LT.pack $ makeRandomString 7 gen
    liftIO $ modifyMVar_ urlMap $ \db -> return (M.insert shortenedUrl url db)
    redirect "/list"

  get "/list" $ do
    db <- liftIO $ readMVar urlMap
    json $ M.toList db

  get "/:hash" $ do
    hash <- param "hash"
    db <- liftIO $ readMVar urlMap
    case M.lookup hash db of
      Nothing -> raise $ mconcat ["Url hash ", LT.pack $ show hash, " not found in database!"]
      Just url -> redirect url

makeRandomString :: RandomGen g => Int -> g -> String
makeRandomString x gen =
  map numToChar $
  take x $
  randomRs (0, length alphabet - 1) gen
  where
    alphabet :: String
    alphabet = '-' : '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

    numToChar :: Int -> Char
    numToChar x = alphabet !! x
