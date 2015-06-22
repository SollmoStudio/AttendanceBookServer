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
import Database.MySQL.Simple
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
------------------------------------------------------------------------------
import Application

successResult :: (ToJSON r) => T.Text -> r -> ByteString
successResult status result = B.toStrict $ encode $ object [ "status" .= status, "result" .= result ]

handleHello :: Handler App App ()
handleHello = writeText "Hi"

handleMakeUser :: Handler App App ()
handleMakeUser = method POST $ do
  email <- getParam "email"
  password <- getParam "password"
  conn <- liftIO $ connect defaultConnectInfo
  liftIO $ execute conn (
    "INSERT  attendance.user SET " `mappend`
    "  email=?, password=?"
    ) (email, password)
  writeBS $ successResult "ok" emptyArray

routes :: [(ByteString, Handler App App ())]
routes = [ ("/hello", handleHello)
         , ("/makeUser", handleMakeUser)
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
