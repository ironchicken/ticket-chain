module TicketChain where

import Control.Exception (Exception)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Time.Clock
import Data.Typeable (Typeable)
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
  { transId :: String
  , transTicket :: Ticket
  , transOrigin :: Maybe Holder
  , transDestination :: Holder
  , transTimestamp :: UTCTime
  , transValue :: Value
  , transOriginSignature :: String
  , transDestinationSignature :: String
  , transPreceding :: Maybe Transaction
  , transHash :: String
  }
  deriving (Show)

instance Eq Transaction where
  (==) a b = transId a == transId b

data Holder = Holder
  { holderIdentity :: String
  , holderPublicKey :: EncryptionKey
  , holderFingerprint :: String
  }
  deriving (Eq, Show)

data Chain = Chain
  { chainHead :: Transaction }
  deriving (Eq, Show)

data ChainException
  = DuplicateTransactionException
  deriving (Show, Typeable)
instance Exception ChainException

verifyHolder :: Chain -> Ticket -> Holder -> Bool
verifyHolder = undefined

verifyTransactionHistory :: Chain -> Ticket -> [(Maybe Holder, Holder, Value)] -> Bool
verifyTransactionHistory chain transfers = undefined

verifySoleTransfer :: Chain -> Ticket -> (Maybe Holder, Holder, Value) -> Bool
verifySoleTransfer chain ticket transfer = undefined

appendTicketTransfer :: Chain -> String -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> EncryptionKey -> UTCTime -> Either ChainException Chain
appendTicketTransfer chain tId ticket value (Just origin) originKey dest destKey time =
  appendTransaction chain transaction
  where
    transaction = signTransactionAsOrigin origin originKey
      $ signTransactionAsDestination dest destKey
      $ transferTicket tId ticket value (Just origin) dest time
appendTicketTransfer chain tId ticket value Nothing _ dest destKey time =
  appendTransaction chain transaction
  where
    transaction = signTransactionAsDestination dest destKey
      $ transferTicket tId ticket value Nothing dest time

transferTicket :: String -> Ticket -> Value -> Maybe Holder -> Holder -> UTCTime -> Transaction
transferTicket tId ticket value origin dest time = Transaction
  { transId = tId
  , transTicket = ticket
  , transOrigin = origin
  , transDestination = dest
  , transTimestamp = time
  , transValue = value
  , transOriginSignature = ""
  , transDestinationSignature = ""
  , transPreceding = Nothing
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
  (show $ transId transaction)
  ++ (show $ transTicket transaction)
  ++ (show $ transTimestamp transaction)
  ++ (show $ transValue transaction)
  ++ (show $ transTimestamp transaction)

appendTransaction :: Chain -> Transaction -> Either ChainException Chain
appendTransaction chain transaction =
  case findTransactionById chain (transId transaction) of
    Just _ -> Left DuplicateTransactionException
    Nothing -> Right $ chain { chainHead = chainedTransaction }
  where
    chainedTransaction =
      transaction { transPreceding = Just oldChainHead }
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
traverseChain f chain = foldChain (\ts t -> (f t) : ts) [] chain

foldChain :: (a -> Transaction -> a) -> a -> Chain -> a
foldChain f initial chain = fold initial (chainHead chain)
  where
    fold acc t@(Transaction { transPreceding = Just p }) = fold (f acc t) p
    fold acc t = f acc t

chainLength :: Chain -> Int
chainLength chain = foldChain (\acc _ -> acc + 1) 0 chain

findTransactionById :: Chain -> String -> Maybe Transaction
findTransactionById chain tId =
  foldChain checkTransaction Nothing chain
  where
    checkTransaction Nothing t
      | transId t == tId = Just t
      | otherwise = Nothing
    checkTransaction (Just t) _ = Just t

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
