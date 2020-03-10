{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types where

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.Writer.Class
import           Data.ByteString
import           Data.Word
import qualified Hasql.Connection           as Connection
import           Servant

data AppConfig =
  AppConfig
    { psql :: PostgreSqlConfig
    }

data PostgreSqlConfig =
  PostgreSqlConfig
    { host     :: ByteString
    , port     :: Word16
    , user     :: ByteString
    , password :: ByteString
    , database :: ByteString
    }

data State =
  State
    { config     :: AppConfig
    , connection :: Connection.Connection
    }

--newtype AppM a =
--  AppM
--    { runApp :: ReaderT State Handler a
--    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader State)

type AppM = ReaderT State Handler

--nt :: State -> AppM a -> Handler a
--nt s x = runReaderT (runApp x) s

makeApp :: State -> AppM a -> Handler a
makeApp state app = runReaderT app state

mkState :: AppConfig -> Connection.Connection -> State
mkState = State

connectionSettings :: PostgreSqlConfig -> Connection.Settings
connectionSettings = Connection.settings <$> host <*> port <*> user <*> password <*> database
