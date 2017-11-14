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
  { ticketId :: String
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

type Transfer = (Maybe Holder, Holder, Value)

data Chain = Chain
  { chainHead :: Transaction }
  deriving (Eq, Show)

data ChainException
  = DuplicateTransactionException
  deriving (Show, Typeable)
instance Exception ChainException

verifyCurrentHolder :: Chain -> Ticket -> Holder -> Bool
verifyCurrentHolder chain ticket holder =
  holder == currentHolder
  where
    (_, currentHolder, _) = head $ reverse $ transferHistory chain ticket

verifyTransferHistory :: Chain -> Ticket -> [Transfer] -> ((Transfer, Transfer) -> Bool) -> Bool
verifyTransferHistory chain ticket transfers eq =
  (length transfers == length history) && (all eq $ zip transfers history)
  where
    history = transferHistory chain ticket

verifySoleTransfer :: Chain -> Ticket -> Transfer -> Bool
verifySoleTransfer chain ticket transfer =
  verifyTransferHistory chain ticket (prependOrigin transfer) holdersMatch
  where
    prependOrigin t@(Just o, _, v) = (Nothing, o, v) : [t]
    prependOrigin t@(Nothing, _, _) = [t]
    holdersMatch ((o1, d1, _), (o2, d2, _)) = o1 == o2 && d1 == d2

verifyIssuer :: Chain -> Ticket -> (Holder, Value) -> Bool
verifyIssuer chain ticket (issuer, value) =
  head (transferHistory chain ticket) == (Nothing, issuer, value)

transferHistory :: Chain -> Ticket -> [Transfer]
transferHistory chain ticket =
  reverse $ orderTransfers Nothing [] transfers
  where
    transactions = findTransactionsForTicket chain ticket
    transfers = map (\t -> (transOrigin t, transDestination t, transValue t)) transactions
    orderTransfers currentHolder ordered (t@(o, d, _):ts)
      | o == currentHolder = orderTransfers (Just d) (t : ordered) transfers
      | otherwise = orderTransfers currentHolder ordered ts
    orderTransfers _ ordered [] = ordered

appendTicketTransfer :: Chain -> String -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> EncryptionKey -> UTCTime -> Either ChainException Chain
appendTicketTransfer chain tId ticket value (Just origin) originKey dest destKey time =
  appendTransaction chain transaction
  where
    transaction = hashTransaction
      $ signTransactionAsOrigin origin originKey
      $ signTransactionAsDestination dest destKey
      $ transferTicket tId ticket value (Just origin) dest time
appendTicketTransfer chain tId ticket value Nothing _ dest destKey time =
  appendTransaction chain transaction
  where
    transaction = hashTransaction
      $ signTransactionAsDestination dest destKey
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

hashTransaction :: Transaction -> Transaction
hashTransaction transaction =
  transaction { transHash = hash $ transPreceding transaction }
  where
    hash (Just preceding) = digest $ transHash preceding ++ serialiseTransaction transaction
    hash Nothing = digest $ serialiseTransaction transaction

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

filterChain :: (Transaction -> Bool) -> Chain -> [Transaction]
filterChain p chain = foldChain filterWithP [] chain
  where
    filterWithP ts t = if p t then t : ts else ts

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
findTransactionsForTicket chain ticket =
  filterChain checkTicket chain
  where
    checkTicket transaction = transTicket transaction == ticket

findTransactionsForHolder :: Chain -> Holder -> [Transaction]
findTransactionsForHolder = undefined

encryptString :: String -> EncryptionKey -> String
encryptString text key = map (\(tc, kc) -> chr $ (ord tc) + (ord kc)) $ zip text (concat $ repeat key)

decryptString :: String -> EncryptionKey -> String
decryptString text key = map (\(tc, kc) -> chr $ (ord tc) - (ord kc)) $ zip text (concat $ repeat key)

signString :: String -> EncryptionKey -> String
signString text key = take 10 $ encryptString text key

digest :: String -> String
digest s = signString s "abc123"
