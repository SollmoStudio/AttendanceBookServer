{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson.Types
import Data.Aeson.Encode
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Database.MySQL.Simple
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
------------------------------------------------------------------------------
import Application
import Config

successResult :: (ToJSON r) => T.Text -> r -> ByteString
successResult status result = B.toStrict $ encode $ object [ "status" .= status, "result" .= result ]

failResult :: T.Text -> ByteString
failResult errorMsg = B.toStrict $ encode $ object [ "status" .= errorMsg, "result" .= emptyArray ]

handleHello :: Handler App App ()
handleHello = writeText "Hi"

handleMakeUser :: Handler App App ()
handleMakeUser = method POST $ do
  email <- getParam "email"
  password <- getParam "password"
  conn <- liftIO $ connect mysqlConnectInfo
  liftIO $ execute conn (
    "INSERT  attendance.user SET " `mappend`
    "  email=?, password=?"
    ) (email, password)
  writeBS $ successResult "ok" emptyArray

validUser :: Handler App App Bool
validUser = do
  email <- getParam "email"
  password <- getParam "password"
  conn <- liftIO $ connect mysqlConnectInfo
  [Only count] <- liftIO $ query conn (
    "SELECT count(*) FROM attendance.user" `mappend`
    " WHERE email=? AND password=?"
    ) (email, password)
  case (count :: Int) of
    0 -> return False
    _ -> return True

saveLoginToSession :: Handler App App ()
saveLoginToSession = do
  (Just email) <- getParam "email"
  let emailBS = decodeUtf8 email
  with sess $ setInSession "email" emailBS
  with sess $ commitSession

handleLogin :: Handler App App ()
handleLogin = method POST $ do
  isValid <- validUser
  case isValid of
    False -> writeBS $ failResult "fail"
    True -> saveLoginToSession >> (writeBS $ successResult "ok" emptyArray)

handleLogout :: Handler App App ()
handleLogout = method POST $ do
  with sess $ resetSession
  with sess $ commitSession
  writeBS $ successResult "ok" emptyArray

routes :: [(ByteString, Handler App App ())]
routes = [ ("/hello", handleHello)
         , ("/makeUser", handleMakeUser)
         , ("/login", handleLogin)
         , ("/logout", handleLogout)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    return $ App s
