
module Config where

import Database.MySQL.Simple

mysqlConnectInfo :: ConnectInfo
mysqlConnectInfo = defaultConnectInfo {
    connectHost = "localhost",
    connectUser = "attendance",
    connectPassword = "attendance",
    connectDatabase = "attendance"
  }

port :: Int
port = 8000
