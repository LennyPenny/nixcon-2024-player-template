module Main where

import Data.Aeson
import Data.Char (ord)
import Data.Text (Text, pack)
import Data.UUID
import Data.UUID.V1 qualified as Data
import Data.UUID.V3 (namespaceDNS)
import Data.UUID.V3 qualified as Data
import Data.Word
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Status (statusFromNat)
import System.IO

-- * api

type ItemApi =
  Get '[PlainText] Text
    :<|> "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer :> Get '[JSON] Item
    :<|> "add" :> Capture "a" Integer :> Capture "b" Integer :> Get '[PlainText] Text
    :<|> "mult" :> Capture "a" Integer :> Capture "b" Integer :> Get '[PlainText] Text
    :<|> "uuid" :> Get '[PlainText] Text

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

main :: IO ()
main = do
  let port = 8080
      settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  home
    :<|> getItems
    :<|> getItemById
    :<|> addNumbers
    :<|> multNumbers
    :<|> uuid

home :: Handler Text
home = return "Hello World!"

addNumbers :: Integer -> Integer -> Handler Text
addNumbers a b = return (Data.Text.pack (show (a + b)))

multNumbers :: Integer -> Integer -> Handler Text
multNumbers a b = return (Data.Text.pack (show (a * b)))

uuid :: Handler Text
uuid = return (Data.Text.pack (show (Data.generateNamed namespaceDNS name)))
  where
    name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item = Item
  { itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item

instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool