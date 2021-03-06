{-# LANGUAGE DeriveGeneric #-}

module TicketChain where

import Control.Exception (Exception)
import Data.Binary
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Time.Clock
import Data.Time.Format
import Data.Typeable (Typeable)
import GHC.Generics
import Network.Socket (HostName, PortNumber)
import Network.TLS hiding (HostName)

type Value = Int
type EncryptionKey = String

data Ticket = Ticket
  { ticketId :: String
  , ticketDescription :: String
  , ticketFaceValue :: Value
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Ticket

data Transaction = Transaction
  { transId :: String
  , transTicket :: Ticket
  , transOrigin :: Maybe Holder
  , transDestination :: Holder
  , transTimestamp :: String
  , transValue :: Value
  , transOriginSignature :: String
  , transDestinationSignature :: String
  , transPreceding :: Maybe (Verified Transaction)
  , transHash :: String
  }
  deriving (Show, Generic, Typeable)

instance Binary Transaction

instance Eq Transaction where
  (==) a b = transId a == transId b

data Unverified a = Unverified a
  deriving (Eq, Show, Generic, Typeable)

instance (Binary a) => Binary (Unverified a)

data Verified a
  = Verified a
  | Unverifiable a
  deriving (Eq, Show, Generic, Typeable)

instance (Binary a) => Binary (Verified a)

instance Functor Verified where
  fmap f (Verified x) = Verified $ f x
  fmap f (Unverifiable x) = Unverifiable $ f x

data Holder = Holder
  { holderIdentity :: String
  , holderPublicKey :: EncryptionKey
  , holderFingerprint :: String
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Holder

type Transfer = (Maybe Holder, Holder, Value)

data Chain = Chain
  { chainHead :: Verified Transaction }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Chain

data ChainException
  = DuplicateTransactionException
  deriving (Show, Typeable)
instance Exception ChainException

data PeerID = PeerID Int BS.ByteString
  deriving (Eq, Show, Generic, Typeable)

instance Binary PeerID

data PeerIDException
  = IncompatiblePeerIDsException
  deriving (Show, Typeable)
instance Exception PeerIDException

data RoutingTable = RoutingTable
  { tableSize :: Int
  , tablePeers :: [(PeerID, HostName, PortNumber)]
  }
  deriving (Eq, Show)

data RoutingTableException
  = RoutingTablePeerSizeMismatchException
  deriving (Show, Typeable)
instance Exception RoutingTableException

data Message
  = FindPeer PeerID
  | JoinNetwork
  | LeaveNetwork
  deriving (Generic, Typeable)

instance Binary Message

data Response
  = FindPeerR PeerID
  | FindPeerFail String
  | JoinNetworkR
  | JoinNetworkFail String
  | LeaveNetworkR
  | LeaveNetworkFail String
  deriving (Generic, Typeable)

instance Binary Response

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
    transfers = map (\(Verified t) -> (transOrigin t, transDestination t, transValue t)) transactions
    orderTransfers currentHolder ordered (t@(o, d, _):ts)
      | o == currentHolder = orderTransfers (Just d) (t : ordered) transfers
      | otherwise = orderTransfers currentHolder ordered ts
    orderTransfers _ ordered [] = ordered

appendTicketTransfer :: Chain -> String -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> EncryptionKey -> UTCTime -> Either ChainException Chain
appendTicketTransfer chain tId ticket value (Just origin) originKey dest destKey time =
  appendTransaction chain transaction
  where
    transaction = hashTransaction <$>
      ( signTransactionAsOrigin origin originKey
      $ signTransactionAsDestination dest destKey
      $ transferTicket tId ticket value (Just origin) dest time )
appendTicketTransfer chain tId ticket value Nothing _ dest destKey time =
  appendTransaction chain transaction
  where
    transaction = hashTransaction <$>
      ( signTransactionAsDestination dest destKey
      $ transferTicket tId ticket value Nothing dest time )

transferTicket :: String -> Ticket -> Value -> Maybe Holder -> Holder -> UTCTime -> Verified Transaction
transferTicket tId ticket value origin dest time = Verified Transaction
  { transId = tId
  , transTicket = ticket
  , transOrigin = origin
  , transDestination = dest
  , transTimestamp = (formatTime defaultTimeLocale rfc822DateFormat time)
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

makeTransaction :: Chain -> Ticket -> Value -> Maybe Holder -> EncryptionKey -> Holder -> Verified Transaction
makeTransaction chain ticket value origin originKey dest = undefined

signTransactionAsOrigin :: Holder -> EncryptionKey -> Verified Transaction -> Verified Transaction
signTransactionAsOrigin _ originKey (Verified transaction) =
  Verified transaction { transOriginSignature = signString (serialiseTransaction transaction) originKey }
signTransactionAsOrigin _ originKey (Unverifiable transaction) =
  Unverifiable transaction { transOriginSignature = signString (serialiseTransaction transaction) originKey }

signTransactionAsDestination :: Holder -> EncryptionKey -> Verified Transaction -> Verified Transaction
signTransactionAsDestination _ destKey (Verified transaction) =
  Verified transaction { transDestinationSignature = signString (serialiseTransaction transaction) destKey }
signTransactionAsDestination _ destKey (Unverifiable transaction) =
  Unverifiable transaction { transDestinationSignature = signString (serialiseTransaction transaction) destKey }

hashTransaction :: Transaction -> Transaction
hashTransaction transaction =
  transaction { transHash = hash $ transPreceding transaction }
  where
    hash (Just (Verified preceding)) = digest $ transHash preceding ++ serialiseTransaction transaction
    hash (Just (Unverifiable preceding)) = digest $ transHash preceding ++ serialiseTransaction transaction
    hash Nothing = digest $ serialiseTransaction transaction

serialiseTransaction :: Transaction -> String
serialiseTransaction transaction =
  (show $ transId transaction)
  ++ (show $ transTicket transaction)
  ++ (show $ transTimestamp transaction)
  ++ (show $ transValue transaction)
  ++ (show $ transTimestamp transaction)

appendTransaction :: Chain -> Verified Transaction -> Either ChainException Chain
appendTransaction chain (Verified transaction) =
  case findTransactionById chain (transId transaction) of
    Just _ -> Left DuplicateTransactionException
    Nothing -> Right $ chain { chainHead = (Verified chainedTransaction) }
  where
    chainedTransaction =
      transaction { transPreceding = Just (Verified oldChainHead) }
    (Verified oldChainHead) = chainHead chain
appendTransaction _ (Unverifiable _) = Left UnverifiableTransactionException

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

joinNetwork :: HostName -> IO PeerID
joinNetwork = undefined

leaveNetwork :: IO ()
leaveNetwork = undefined

grantPeerID :: HostName -> IO PeerID
grantPeerID = undefined

peerDistance :: PeerID -> PeerID -> Either PeerIDException BS.ByteString
peerDistance (PeerID size1 id1) (PeerID size2 id2)
  | size1 == size2 = Right $ BS.pack $ BS.zipWith xor id1 id2
  | otherwise = Left IncompatiblePeerIDsException

addToRoutingTable :: RoutingTable -> (PeerID, HostName, PortNumber) -> Either RoutingTableException RoutingTable
addToRoutingTable routes newPeer@((PeerID peerSize _), _, _)
  | (tableSize routes) == peerSize = Right $ routes { tablePeers = (tablePeers routes) ++ [newPeer] }
  | otherwise = Left RoutingTablePeerSizeMismatchException

removeFromRoutingTable :: RoutingTable -> PeerID -> Either RoutingTableException RoutingTable
removeFromRoutingTable routes peerId@(PeerID size _)
  | (tableSize routes) == size = Right $ routes { tablePeers = (filter (\(pid, _, _) -> pid /= peerId) $ tablePeers routes) }
  | otherwise = Left RoutingTablePeerSizeMismatchException

findClosest :: RoutingTable -> PeerID -> PeerID
findClosest (RoutingTable { tablePeers = ps }) target =
  foldr closest (getId $ head ps) ps
  where
    closest peer current
      | distToTarget < distToCurrent = getId peer
      | otherwise = current
      where
        Right distToTarget = peerDistance (getId peer) target
        Right distToCurrent = peerDistance (getId peer) current
    getId (peerId, _, _) = peerId

nextPeer :: PeerID -> PeerID
nextPeer (PeerID size bytes) =
  PeerID size nextBytes
  where
    mkId pos = BS.pack $ (take (size - pos - 1) $ repeat 0) ++ [maxBound :: Word8] ++ (take pos $ repeat 0)
    followingIds = filter (\pid -> pid >= bytes) $ map mkId [0..size - 1]
    nextBytes
      | null followingIds = mkId (size - 1)
      | otherwise = head followingIds

previousPeer :: PeerID -> PeerID
previousPeer (PeerID size bytes) =
  PeerID size nextBytes
  where
    mkId pos = BS.pack $ (take (size - pos - 1) $ repeat 0) ++ [maxBound :: Word8] ++ (take pos $ repeat 0)
    precedingIds = filter (\pid -> pid <= bytes) $ map mkId $ reverse [0..size - 1]
    nextBytes
      | null precedingIds = BS.pack $ take size $ repeat 0
      | otherwise = head precedingIds

findPeer :: (Message -> PeerID -> IO Response) -> RoutingTable -> PeerID -> IO PeerID
findPeer sendMsgAction routes query =
  findTarget Nothing $ findClosest routes query
  where
    findTarget Nothing candidate
      | candidate == query = return candidate
      | otherwise = askCandidate candidate
    findTarget (Just lastAsked) candidate
      | candidate == query = return candidate
      | candidate == lastAsked = return lastAsked
      | otherwise =  askCandidate candidate

    askCandidate candidate = do
      next <- sendMsgAction (FindPeer query) candidate
      case next of
        FindPeerR responseId -> findTarget (Just candidate) responseId
        FindPeerFail msg -> fail $ "FindPeer failed: " ++ msg
        _ -> fail "Invalid response to FindPeer message"

pingPeer :: PeerID -> Int -> IO Bool
pingPeer peerId timeout = undefined

sendMessage :: Message -> PeerID -> IO Response
sendMessage = undefined

refreshRoutingTable :: RoutingTable -> IO RoutingTable
refreshRoutingTable = undefined

pullChain :: HostName -> IO Chain
pullChain = undefined

pushChain :: Chain -> [HostName] -> IO ()
pushChain = undefined

traverseChain :: (Verified Transaction -> a) -> Chain -> [a]
traverseChain f chain = foldChain (\ts t -> (f t) : ts) [] chain

foldChain :: (a -> Verified Transaction -> a) -> a -> Chain -> a
foldChain f initial chain = fold initial (chainHead chain)
  where
    fold acc t@(Verified Transaction { transPreceding = Just p }) = fold (f acc t) p
    fold acc t = f acc t

filterChain :: (Verified Transaction -> Bool) -> Chain -> [Verified Transaction]
filterChain p chain = foldChain filterWithP [] chain
  where
    filterWithP ts t = if p t then t : ts else ts

chainLength :: Chain -> Int
chainLength chain = foldChain (\acc _ -> acc + 1) 0 chain

findTransactionById :: Chain -> String -> Maybe (Verified Transaction)
findTransactionById chain tId =
  foldChain checkTransaction Nothing chain
  where
    checkTransaction Nothing (Verified t)
      | transId t == tId = Just (Verified t)
      | otherwise = Nothing
    checkTransaction Nothing (Unverifiable _) = Nothing
    checkTransaction (Just t) _ = Just t

findTransactionsForTicket :: Chain -> Ticket -> [Verified Transaction]
findTransactionsForTicket chain ticket =
  filterChain checkTicket chain
  where
    checkTicket (Verified transaction) = transTicket transaction == ticket
    checkTicket (Unverifiable _) = False

findTransactionsForHolder :: Chain -> Holder -> [Verified Transaction]
findTransactionsForHolder = undefined

encryptString :: String -> EncryptionKey -> String
encryptString text key = map (\(tc, kc) -> chr $ (ord tc) + (ord kc)) $ zip text (concat $ repeat key)

decryptString :: String -> EncryptionKey -> String
decryptString text key = map (\(tc, kc) -> chr $ (ord tc) - (ord kc)) $ zip text (concat $ repeat key)

signString :: String -> EncryptionKey -> String
signString text key = take 10 $ encryptString text key

digest :: String -> String
digest s = signString s "abc123"
