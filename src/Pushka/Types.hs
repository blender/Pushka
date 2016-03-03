{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Pushka.Types where

import qualified Data.Text        as T
import           Database.MongoDB as MN
import           Control.Monad.IO.Class



type UserId       = String
type DeviceToken  = String

data UserDevice  = UserDevice { userId      :: UserId
                              , deviceToken :: DeviceToken
                              }
                              deriving (Show)



toDocument :: UserDevice -> Document
toDocument UserDevice { .. } = ["userId"      =: T.pack userId
                              , "deviceToken" =: T.pack deviceToken]


insertUserDeviceAction:: (MonadIO m) => UserDevice -> Action m Value
insertUserDeviceAction ud = insert "device_tokens" $ toDocument ud
