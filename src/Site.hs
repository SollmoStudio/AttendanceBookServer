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
import Data.Time.Clock
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

existUser :: T.Text -> Handler App App Bool
existUser email = do
  conn <- liftIO $ connect mysqlConnectInfo
  [Only count] <- liftIO $ query conn (
    "SELECT count(*) FROM attendance.user" `mappend`
    " WHERE email=?"
    ) (Only email)
  case (count :: Int) of
    0 -> return False
    _ -> return True

isLogin :: Handler App App Bool
isLogin = do
  maybeEmail <- with sess $ getFromSession "email"
  ret <- maybe (return False) existUser maybeEmail
  return ret

handleIsLogin :: Handler App App ()
handleIsLogin = do
  logined <- isLogin
  writeBS $ successResult "ok" [logined]

checkLogin :: Handler App App ()
checkLogin = do
  logined <- isLogin
  if logined then pass else writeBS $ failResult "NotLogin"

postAttend :: Handler App App ()
postAttend = method POST $ do
  conn <- liftIO $ connect mysqlConnectInfo
  (Just email) <- with sess $ getFromSession "email"
  liftIO $ execute conn (
    "insert  attendance.attendance SET " `mappend`
    "  email=?"
    ) (Only email)
  writeBS $ successResult "ok" emptyArray

newtype AttendanceResult = AttendanceResult [(T.Text, UTCTime)]
  deriving Show

fromSingle :: (T.Text, UTCTime) -> AttendanceResult
fromSingle x = AttendanceResult [x]

instance Monoid AttendanceResult where
  mempty = AttendanceResult []
  mappend (AttendanceResult xs) (AttendanceResult ys) = AttendanceResult (xs `mappend` ys)

getAttend :: Handler App App ()
getAttend = method GET $ do
  conn <- liftIO $ connect mysqlConnectInfo
  (Just email) <- with sess $ getFromSession "email"
  attendances <- liftIO $ query conn (
       "select email, attendanceTime from attendance.attendance WHERE " `mappend`
       "  email=?"
     ) (Only email)
  let AttendanceResult results = foldMap fromSingle attendances
  writeBS $ successResult "ok" results

handleAttend :: Handler App App ()
handleAttend = checkLogin <|> (postAttend <|> getAttend)

handleAttends :: Handler App App ()
handleAttends = do
  conn <- liftIO $ connect mysqlConnectInfo
  attendances <- liftIO $ query_ conn (
    "select email, attendanceTime from attendance.attendance"
    )
  let AttendanceResult results = foldMap fromSingle attendances
  writeBS $ successResult "ok" results

routes :: [(ByteString, Handler App App ())]
routes = [ ("/hello", handleHello)
         , ("/makeUser", handleMakeUser)
         , ("/login", handleLogin)
         , ("/logout", handleLogout)
         , ("/isLogin", handleIsLogin)
         , ("/attend", handleAttend)
         , ("/attends", handleAttends)
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
