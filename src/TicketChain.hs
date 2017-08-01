module TicketChain where

import Data.Time.Clock

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

verifyHolder :: Chain -> Ticket -> Bool
verifyHolder = undefined

verifyTransactionHistory :: Chain -> Ticket -> [(Maybe Holder, Holder, Value)] -> Bool
verifyTransactionHistory chain transfers = undefined

verifySoleTransfer :: Chain -> Ticket -> (Maybe Holder, Holder, Value) -> Bool
verifySoleTransfer chain ticket transfer = undefined

transferTicket :: Chain -> Ticket -> Value -> Maybe Holder -> Holder -> Chain
transferTicket chain ticket value origin dest = undefined

makeTransaction :: Chain -> Ticket -> Value -> Maybe Holder -> Holder -> Transaction
makeTransaction chain ticket value origin dest = undefined

signTransaction :: Transaction -> Transaction
signTransaction = undefined

loadPrivateKey :: IO EncryptionKey
loadPrivateKey = undefined

loadChain :: FilePath -> IO Chain
loadChain = undefined

writeChain :: FilePath -> Chain -> IO ()
writeChain = undefined

pullChain :: IO Chain
pullChain = undefined

pushChain :: Chain -> IO ()
pushChain = undefined

traverseChain :: (Transaction -> a) -> Chain -> [a]
traverseChain = undefined

foldChain :: (a -> Transaction -> a) -> a -> Chain -> a
foldChain = undefined

findTransactionsForTicket :: Chain -> Ticket -> [Transaction]
findTransactionsForTicket = undefined

findTransactionsForHolder :: Chain -> Holder -> [Transaction]
findTransactionsForHolder = undefined
