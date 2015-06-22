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
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.Text as T
import Database.MySQL.Simple
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
------------------------------------------------------------------------------
import Application

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
  writeBS $ "ok"

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
