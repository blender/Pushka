{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Main where

import qualified Web.Spock.Safe               as S
import           Web.Spock.Shared

import           Control.Arrow
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.AWS      as TASW
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Monoid

import qualified Database.MongoDB             as DB

import qualified Network.AWS                  as AWS
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Pushka.Types

-- spockT :: MonadIO m => (forall a. m a -> IO a) -> SpockT m () -> IO Middleware
-- type SpockT = SpockCtxT ()
-- data SpockCtxT ctx m a

-- spock :: SpockCfg conn sess st -> SpockM conn sess st () -> IO Middleware
-- type SpockM conn sess st = SpockCtxM () conn sess st
-- type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)
-- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> SpockCfg conn sess st


-- curl -X GET -H "Authorization: Bearer someUserID_5185415ba171ea3a00704eed" http://localhost:8080/register?deviceToken=some_device_token

mongoPoolCfg :: S.PoolCfg
mongoPoolCfg = PoolCfg 10 10 10


main :: IO ()
main =
  S.runSpock 8080 $ S.spock spockCgf runPushka
  where
    connection =  DB.connect $ DB.Host "192.168.99.100" (DB.PortNumber 32769)
    connector  = S.ConnBuilder connection DB.close mongoPoolCfg
    spockCgf = defaultSpockCfg Nothing (PCConn connector) ()

runPushka = do
    liftIO $
         putStrLn "======================================================="
      >> putStrLn "                         Пушка                         "
      >> putStrLn "======================================================="
      >> putStrLn ""

    S.prehook authHook $ do

      httpsRequestManger <- liftIO $ newManager tlsManagerSettings

      liftIO $ putStrLn "Connecting to Mongo..."
      collections <- runQuery testPipe
      liftIO $ mapM_ (putStrLn . T.unpack)  collections
      liftIO $ putStrLn $ "Connected: there are " ++ show (length collections) ++ " collections"

      S.get "register" $ registerUserDevice httpsRequestManger

      S.post ("pushTo" S.<//> S.var) $ pushToUser httpsRequestManger


-- Maybe use a Reader Here? they all take manager as the first param

registerUserDevice :: Manager -> S.SpockActionCtx (Maybe (T.Text, T.Text)) DB.Pipe session state ()
registerUserDevice requestManager = do
  maybeUserIdAndToken <- getContext
  case maybeUserIdAndToken of
    Just (userId, token) -> do

      maybeDeviceToken :: Maybe String <- param "deviceToken"

      case maybeDeviceToken of
        Just deviceToken -> do

          liftIO $ putStrLn ""
            >> putStrLn "=============== BEARER SUCCESSFULL ===================="
            >> putStrLn ("Will try to validate: "
              <> T.unpack userId
              <> " with token: "
              <> T.unpack token)
            >> putStrLn "======================================================="


          request <- authRequestForUser userId token
          response <- lift $ httpLbs request requestManager

          liftIO $ putStrLn ""
                >> putStrLn "==================== SSO RESPONSE ====================="
                >> print response
                >> putStrLn "======================================================="


          if  responseStatus response == ok200
            then do
              value <- runQuery $ insertUserDevice UserDevice { userId = T.unpack userId, deviceToken = deviceToken}
              liftIO . print $  "Added UserDevice : " ++ show value ++ ""
              respondOK "Hooray!"
            else respondUnauthorizedRequest


        Nothing -> respondBadRequest
      where
        idAndToken = T.unpack $ userId <> T.pack " " <> token

    Nothing -> respondUnauthorizedRequest

pushToUser :: Manager -> UserId -> S.SpockActionCtx (Maybe (T.Text, T.Text)) DB.Pipe session state ()
pushToUser requestManager userId = do
  maybeUserIdAndToken <- getContext
  case maybeUserIdAndToken of
      Just (userId, token) -> respondOK $ T.unpack userId
      Nothing -> respondBadRequest

respondBadRequest :: MonadIO m => S.ActionCtxT ctx m ()
respondBadRequest = respond badRequest400 "Bad Request"

respondUnauthorizedRequest :: MonadIO m => S.ActionCtxT ctx m ()
respondUnauthorizedRequest = respond unauthorized401 "Authorization Failed"

respond :: MonadIO m => Status -> String -> S.ActionCtxT ctx m ()
respond status message = setStatus status >> json message

respondOK :: MonadIO m => String -> S.ActionCtxT ctx m ()
respondOK = respond ok200

authHook = do
  oldCtx <- S.getContext
  authHeader <- S.header "Authorization"
  return $ authHeader
    >>= firstWordEqualTo "bearer"
    >>= dropLenghtOf "bearer"
    >>= splitIdAndAccessToken

firstWordEqualTo :: String -> T.Text -> Maybe T.Text
firstWordEqualTo s text = if s `isEqualTo` text
  then Just text
  else Nothing
  where
    isEqualTo k = T.words >>> head >>> T.toLower >>> (== T.pack k)

dropLenghtOf :: String -> T.Text -> Maybe T.Text
dropLenghtOf t1 t2 = if T.null text
  then Nothing
  else Just $ T.strip text
  where
    text = T.drop (length t1) t2


{-|
  WTF is going on here, this is some crazy logic for splitting the token
  ported form the Java server. Whoever though that it was ok to use '_'
  as the joining element for userId and token should have prevended
  '_' from appering elsewhere. Sadly this is not the case.
-}
splitIdAndAccessToken :: T.Text -> Maybe (T.Text, T.Text)
splitIdAndAccessToken text =
  if length tokens >= 2
  then
    if (T.length . head $ tokens) == 4
      then pure (T.intercalate "_" . take 2 $ tokens , T.intercalate "_" . drop 2 $ tokens)
      else pure (head tokens, T.intercalate "_" . tail $ tokens)
  else Nothing
  where
    tokens = T.split (=='_') text

authRequestForUser :: (MonadIO m) => T.Text -> T.Text -> m Request
authRequestForUser userId token = liftIO $ do
  initReq <- parseUrl $ "http://www.mocky.io/v2/" <> T.unpack token <> "?grant_type=inline-bearer"
  return $ initReq { method = methodGet
    , requestHeaders = [ basicAuthHeader ] }
    where
      basicAuthHeader = (hAuthorization , "Basic" <> " " <> "c3RyZXNzLW1ncjorUFlxYFcyWnRCPnVCW2g=")

-- runQuery :: (SpockConn m -> IO a) -> m a
testPipe :: DB.Pipe -> IO [T.Text]
testPipe pipe = runMongo "master" pipe DB.allCollections

insertUserDevice :: UserDevice -> DB.Pipe -> IO DB.Value
insertUserDevice ud pipe = runMongo "master" pipe (insertUserDeviceAction ud)

runMongo :: (MonadIO m) => String -> DB.Pipe -> DB.Action m a -> m a
runMongo dbName pipe = DB.access pipe DB.master (T.pack dbName)
