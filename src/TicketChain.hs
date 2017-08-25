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

appendTicketTransfer :: Chain -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> EncryptionKey -> UTCTime -> Chain
appendTicketTransfer chain ticket value (Just origin) originKey dest destKey time =
  appendTransaction chain transaction
  where
    transaction = signTransactionAsOrigin origin originKey
      $ signTransactionAsDestination dest destKey
      $ transferTicket ticket value (Just origin) dest time
appendTicketTransfer chain ticket value Nothing _ dest destKey time =
  appendTransaction chain transaction
  where
    transaction = signTransactionAsDestination dest destKey
      $ transferTicket ticket value Nothing dest time

transferTicket :: Ticket -> Value -> Maybe Holder -> Holder -> UTCTime -> Transaction
transferTicket ticket value origin dest time = Transaction
  { transTicket = ticket
  , transOrigin = origin
  , transDestination = dest
  , transTimestamp = time
  , transValue = value
  , transOriginSignature = ""
  , transDestinationSignature = ""
  , transPreceding = Nothing
  , transFollowing = Nothing
  , transHash = ""
  }

presentTicket :: Chain -> Ticket -> Holder -> EncryptionKey -> Holder -> EncryptionKey -> Bool
presentTicket chain ticket origin originKey dest destKey = undefined

detachStub :: Chain -> Ticket -> (Ticket, Ticket)
detachStub = undefined

splitTicket :: Chain -> Ticket -> (Ticket, Ticket)
splitTicket = undefined

makeTransaction :: Chain -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> Transaction
makeTransaction chain ticket value origin originKey dest = undefined

signTransactionAsOrigin :: Holder -> EncryptionKey -> Transaction -> Transaction
signTransactionAsOrigin _ originKey transaction =
  transaction { transOriginSignature = signString (serialiseTransaction transaction) originKey }

signTransactionAsDestination :: Holder -> EncryptionKey -> Transaction -> Transaction
signTransactionAsDestination _ destKey transaction =
  transaction { transDestinationSignature = signString (serialiseTransaction transaction) destKey }

serialiseTransaction :: Transaction -> String
serialiseTransaction transaction =
  (show $ transTicket transaction)
  ++ (show $ transTimestamp transaction)
  ++ (show $ transValue transaction)
  ++ (show $ transTimestamp transaction)

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
encryptString text key = map (\(tc, kc) -> chr $ (ord tc) + (ord kc)) $ zip text (concat $ repeat key)

decryptString :: String -> EncryptionKey -> String
decryptString text key = map (\(tc, kc) -> chr $ (ord tc) - (ord kc)) $ zip text (concat $ repeat key)

signString :: String -> EncryptionKey -> String
signString text key = take 10 $ encryptString text key
