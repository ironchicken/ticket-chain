module TicketChain where

import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Time.Clock
import Network.TLS

type Value = Int
type EncryptionKey = String

data Ticket = Ticket
  { ticketId :: Int
  , ticketDescription :: String
  , ticketFaceValue :: Value
  }
  deriving (Eq, Show)

data Transaction = Transaction
  { transTicket :: Ticket
  , transOrigin :: Maybe Holder
  , transDestination :: Holder
  , transTimestamp :: UTCTime
  , transValue :: Value
  , transOriginSignature :: String
  , transDestinationSignature :: String
  , transPreceding :: Maybe Transaction
  , transFollowing :: Maybe Transaction
  , transHash :: String
  }
  deriving (Eq, Show)

data Holder = Holder
  { holderIdentity :: String
  , holderPublicKey :: EncryptionKey
  , holderFingerprint :: String
  }
  deriving (Eq, Show)

data Chain = Chain
  { chainHead :: Transaction }
  deriving (Eq, Show)

verifyHolder :: Chain -> Ticket -> Holder -> Bool
verifyHolder = undefined

verifyTransactionHistory :: Chain -> Ticket -> [(Maybe Holder, Holder, Value)] -> Bool
verifyTransactionHistory chain transfers = undefined

verifySoleTransfer :: Chain -> Ticket -> (Maybe Holder, Holder, Value) -> Bool
verifySoleTransfer chain ticket transfer = undefined

transferTicket :: Chain -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> EncryptionKey -> Chain
transferTicket chain ticket value origin originKey dest destKey = undefined

presentTicket :: Chain -> Ticket -> Holder -> EncryptionKey -> Holder -> EncryptionKey -> Bool
presentTicket chain ticket origin originKey dest destKey = undefined

detachStub :: Chain -> Ticket -> (Ticket, Ticket)
detachStub = undefined

splitTicket :: Chain -> Ticket -> (Ticket, Ticket)
splitTicket = undefined

makeTransaction :: Chain -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> Transaction
makeTransaction chain ticket value origin originKey dest = undefined

signTransaction :: Transaction -> Transaction
signTransaction = undefined

appendTransaction :: Chain -> Transaction -> Chain
appendTransaction chain transaction =
  chain { chainHead = chainedTransaction }
  where
    chainedTransaction =
      transaction { transPreceding = Just oldChainHead
                  , transFollowing = Nothing
                  }
    oldChainHead = chainHead chain

loadPrivateKey :: IO EncryptionKey
loadPrivateKey = undefined

serialiseChain :: Chain -> BS.ByteString
serialiseChain = undefined

unserialiseChain :: BS.ByteString -> Chain
unserialiseChain = undefined

encryptChain :: BS.ByteString -> EncryptionKey -> BS.ByteString
encryptChain = undefined

decryptChain :: BS.ByteString -> EncryptionKey -> BS.ByteString
decryptChain = undefined

loadChain :: FilePath -> IO Chain
loadChain = undefined

writeChain :: FilePath -> Chain -> IO ()
writeChain = undefined

wellKnownPeers :: [HostName]
wellKnownPeers = []

findPeers :: IO [HostName]
findPeers = undefined

pullChain :: HostName -> IO Chain
pullChain = undefined

pushChain :: Chain -> [HostName] -> IO ()
pushChain = undefined

traverseChain :: (Transaction -> a) -> Chain -> [a]
traverseChain = undefined

foldChain :: (a -> Transaction -> a) -> a -> Chain -> a
foldChain = undefined

findTransactionsForTicket :: Chain -> Ticket -> [Transaction]
findTransactionsForTicket = undefined

findTransactionsForHolder :: Chain -> Holder -> [Transaction]
findTransactionsForHolder = undefined

encryptString :: String -> EncryptionKey -> String
encryptString text _ = map (\c -> chr $ (ord c) + 1) text

decryptString :: String -> EncryptionKey -> String
decryptString text _ = map (\c -> chr $ (ord c) - 1) text

signString :: String -> EncryptionKey -> String
signString text _ = map (\c -> chr $ (ord c) + 1) $ take 10 text
