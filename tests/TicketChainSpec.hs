module TicketChainSpec
  (spec)
where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Word (Word8)
import Network.Socket (HostName, PortNumber)
import Test.Hspec
import Test.HUnit.Lang
import TicketChain

spec :: Spec
spec = do
  describe "appendTransaction" $ do
    it "should update the head of the given chain" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          newTransaction =
            (fakeTransaction fakeTicket Nothing fakeHolder laterTime) { transId = "second" }
          laterTime =
            UTCTime
            { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
            , utctDayTime = secondsToDiffTime 60
            }
          updatedChain =
            appendTransaction initialChain newTransaction
      updatedChain `rightShouldSatisfy` (\c -> (transTimestamp $ chainHead c) == transTimestamp newTransaction)

    it "should not append the transaction if its ID clashes with a transaction already in the chain" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "same" }
          newTransaction =
            (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "same" }
          updatedChain =
            appendTransaction initialChain newTransaction
      shouldBeLeft updatedChain

  describe "traverseChain" $ do
    it "should apply the function to every transaction in the chain" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (length (traverseChain id chain)) `shouldBe` 2

  describe "filterChain" $ do
    it "should return a list of transactions which satisfy the predicate" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (length (filterChain (\t -> transId t == "first") chain)) `shouldBe` 1

    it "should return an empty list given none of the transactions satisfy the predicate" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (length (filterChain (\t -> transId t == "third") chain)) `shouldBe` 0

  describe "findTransactionById" $ do
    it "should return just a transaction if one matching the given ID exists in the chain" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (findTransactionById chain "first") `shouldBe` (Just trans1)

    it "should return nothing if there are no transactions in the chain matching the given ID" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (findTransactionById chain "third") `shouldBe` Nothing

  describe "findTransactionsForTicket" $ do
    it "should return a list of transactions for the given ticket" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      (length (findTransactionsForTicket chain fakeTicket)) `shouldBe` 2

    it "should return an empty list given no transactions exist for the given ticket" $ do
      let (Right chain) =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
          otherTicket = fakeTicket { ticketId = "2" }
      (length (findTransactionsForTicket chain otherTicket)) `shouldBe` 0

  describe "chainLength" $ do
    it "should count the number of transactions in the chain" $ do
      let chain = Chain { chainHead = trans1 }
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
      chainLength chain `shouldBe` 1

  describe "appendTicketTransfer" $ do
    it "should create a new transaction" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            Ticket
            { ticketId = "2"
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      (transTicket $ chainHead updatedChain) `shouldBe` newTicket
      (transValue $ chainHead updatedChain) `shouldBe` newValue

    it "should sign the transaction with the origin holder key" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            fakeTicket { ticketId = "2" }
          newTransId = "new-trans-id"
          origin =
            Just fakeHolder
            { holderIdentity = "origin"
            , holderPublicKey = "origin-pub-key"
            }
          originKey =
            "origin-priv-key"
          ticketDigest t =
            signString (serialiseTransaction t) originKey
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket 0 origin originKey fakeHolder "key" fakeUTCTime
      (transOriginSignature $ chainHead updatedChain) `shouldBe` (ticketDigest $ chainHead updatedChain)

    it "should sign the transaction with the destination holder key" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            fakeTicket { ticketId = "2" }
          newTransId = "new-trans-id"
          destination =
            fakeHolder
            { holderIdentity = "destination"
            , holderPublicKey = "destination-pub-key"
            }
          destinationKey =
            "destination-priv-key"
          ticketDigest t =
            signString (serialiseTransaction t) destinationKey
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket 0 Nothing "" destination destinationKey fakeUTCTime
      (transDestinationSignature $ chainHead updatedChain) `shouldBe` (ticketDigest $ chainHead updatedChain)

    it "should update the head of the given chain" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            Ticket
            { ticketId = "2"
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      (chainHead updatedChain) `shouldNotBe` existingTransaction

    it "should leave only one transaction for the ticket given there is no origin holder" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            Ticket
            { ticketId = "2"
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      (length (findTransactionsForTicket updatedChain newTicket)) `shouldBe` 1

    it "should hash the new transaction" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            Ticket
            { ticketId = "2"
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          (Right updatedChain) =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      (transHash $ chainHead updatedChain) `shouldNotBe` ""

  describe "transferHistory" $ do
    it "should return all the correctly chained transfers relating to the given ticket" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3

      (transferHistory chain3 fakeTicket) `shouldBe` [(Nothing, h1, 0), (Just h1, h2, 1), (Just h2, h3, 1)]

    it "should return an empty list if no transfers exist for the given ticket" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3
          differentTicket =
            Ticket
            { ticketId = "2"
            , ticketDescription = "Ticket"
            , ticketFaceValue = 1
            }

      (transferHistory chain3 differentTicket) `shouldBe` []

    it "should return the origin transaction first" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time3) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time1
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time2

      (head $ transferHistory chain3 fakeTicket) `shouldBe` (Nothing, h1, 0)

    it "should order the transactions by the transfer of ownership" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3
          (Right chain3) =
            appendTicketTransfer chain2 "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2

      (transferHistory chain3 fakeTicket) `shouldBe` [(Nothing, h1, 0), (Just h1, h2, 1), (Just h2, h3, 1)]

    it "should return only non-sparse chains of ownership" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h2) "key2" h3 "key3" time2

      (transferHistory chain2 fakeTicket) `shouldBe` [(Nothing, h1, 0)]

    it "should return an empty list given the origin transaction is not in the ticket chain" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket (Just h1) h2 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h2) "key2" h3 "key3" time2

      (transferHistory chain2 fakeTicket) `shouldBe` []

  describe "verifyCurrentHolder" $ do
    it "should return True if the given ticket has the given holder" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3

      (verifyCurrentHolder chain3 fakeTicket h3) `shouldBe` True

    it "should return False if the given ticket does not have the given holder" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3

      (verifyCurrentHolder chain3 fakeTicket h1) `shouldBe` False

  describe "verifyTransferHistory" $ do
    it "should return True if the given transfer history matches the transfer history for the given ticket" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3
          eqPredicate (a, b) = a == b

      (verifyTransferHistory chain3 fakeTicket [(Nothing, h1, 0), (Just h1, h2, 1), (Just h2, h3, 1)] eqPredicate) `shouldBe` True

    it "should return True if the given transfer history matches the transfer history for the given ticket according to the given predicate" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3
          predicate ((o1, d1, v1), (o2, d2, v2)) = o1 == o2 && d1 == d2 && v1 == v2

      (verifyTransferHistory chain3 fakeTicket [(Nothing, h1, 0), (Just h1, h2, 1), (Just h2, h3, 1)] predicate) `shouldBe` True

    it "should return False if the given transfer history does not match the transfer history for the given ticket" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3
          eqPredicate (a, b) = a == b

      (verifyTransferHistory chain3 fakeTicket [(Nothing, h2, 0), (Just h2, h3, 1), (Just h3, h1, 1)] eqPredicate) `shouldBe` False

  describe "verifySoleTransfer" $ do
    it "should return True if the given transfer is the only transfer for the given ticket after it was added to the chain" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2

      (verifySoleTransfer chain2 fakeTicket (Just h1, h2, 1)) `shouldBe` True

    it "should return False if there is more than one transfer for the given ticket after it was added to the chain" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          h3 = Holder { holderIdentity = "holder3", holderPublicKey = "key3", holderFingerprint = "finger-print3" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          time3 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10002 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first" }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2
          (Right chain3) =
            appendTicketTransfer chain2 "third" fakeTicket 1 (Just h2) "key2" h3 "key3" time3

      (verifySoleTransfer chain3 fakeTicket (Just h1, h2, 1)) `shouldBe` False

  describe "verifyIssuer" $ do
    it "should return True if the given ticket was issued by the given holder at the given value" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first", transValue = 1 }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2

      (verifyIssuer chain2 fakeTicket (h1, 1)) `shouldBe` True

    it "should return False if the given ticket was not issued by the given holder" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first", transValue = 1 }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2

      (verifyIssuer chain2 fakeTicket (h2, 1)) `shouldBe` False

    it "should return False if the given ticket was not issued at the given value" $ do
      let initialChain = Chain { chainHead = t1 }
          h1 = Holder { holderIdentity = "holder1", holderPublicKey = "key1", holderFingerprint = "finger-print1" }
          h2 = Holder { holderIdentity = "holder2", holderPublicKey = "key2", holderFingerprint = "finger-print2" }
          time1 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10000 }, utctDayTime = secondsToDiffTime 60 }
          time2 = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 10001 }, utctDayTime = secondsToDiffTime 60 }
          t1 = (fakeTransaction fakeTicket Nothing h1 time1) { transId = "first", transValue = 1 }
          (Right chain2) =
            appendTicketTransfer initialChain "second" fakeTicket 1 (Just h1) "key1" h2 "key2" time2

      (verifyIssuer chain2 fakeTicket (h1, 0)) `shouldBe` False

  describe "peerDistance" $ do
    it "should return the XOR between the two given peer IDs" $ do
      let id1 = BS.pack $ take 4 $ repeat (0 :: Word8)
          id2 = BS.pack $ take 3 $ repeat (0 :: Word8) ++ [(255 :: Word8)]
          pid1 = PeerID 4 id1
          pid2 = PeerID 4 id2
          expDistance = BS.pack $ BS.zipWith xor id1 id2
          Right actDistance = peerDistance pid1 pid2

      actDistance `shouldBe` expDistance

    it "should return a PeerIDException if the given peer IDs are of different lengths" $ do
      let id1 = BS.pack $ take 4 $ repeat (0 :: Word8)
          id2 = BS.pack $ take 6 $ repeat (0 :: Word8)
          pid1 = PeerID 4 id1
          pid2 = PeerID 6 id2

      shouldBeLeft (peerDistance pid1 pid2)

  describe "addToRoutingTable" $ do
    it "should add the given peer to the given routing table" $ do
      let initialRoutes = RoutingTable { tableSize = 4, tablePeers = [] }
          newPeer = PeerID 4 (BS.pack $ take 4 $ repeat (0 :: Word8))
          Right newRoutes = addToRoutingTable initialRoutes (newPeer, "localhost", 1 :: PortNumber)

      (length $ tablePeers newRoutes) `shouldBe` 1

    it "should return a RoutingTablePeerSizeMismatchException if the given peer ID does not match the size of the given routing table" $ do
      let initialRoutes = RoutingTable { tableSize = 6, tablePeers = [] }
          newPeer = PeerID 4 (BS.pack $ take 4 $ repeat (0 :: Word8))

      shouldBeLeft (addToRoutingTable initialRoutes (newPeer, "localhost", 1 :: PortNumber))

  describe "findClosest" $ do
    it "should return the peer in the given routing table with the smallest distance from the given peer" $ do
      let routes = RoutingTable { tableSize = 4, tablePeers = ps }
          ps = [ (p2, "192.168.0.1", 1 :: PortNumber)
               , (p3, "192.168.0.2", 1 :: PortNumber)
               , (p4, "192.168.0.3", 1 :: PortNumber)
               ]
          p1 = PeerID 4 (BS.pack [0,   0,   0,   0])
          p2 = PeerID 4 (BS.pack [0,   0,   255, 0])
          p3 = PeerID 4 (BS.pack [0,   255, 0,   0])
          p4 = PeerID 4 (BS.pack [255, 0,   0,   0])

      (findClosest routes p1) `shouldBe` p2

  describe "nextPeer" $ do
    it "should return the cell peer ID following the given peer ID" $ do
      let p1 = PeerID 4 (BS.pack [0, 0, 0, 0])
          p1Next = PeerID 4 (BS.pack [0, 0, 0, 255])
          p2 = PeerID 4 (BS.pack [0, 0, 0, 1])
          p2Next = PeerID 4 (BS.pack [0, 0, 0, 255])
          p3 = PeerID 4 (BS.pack [0, 1, 0, 0])
          p3Next = PeerID 4 (BS.pack [0, 255, 0, 0])
          p4 = PeerID 4 (BS.pack [1, 0, 0, 0])
          p5 = PeerID 4 (BS.pack [254, 255, 255, 255])
          p4And5Next = PeerID 4 (BS.pack [255, 0, 0, 0])

      (nextPeer p1) `shouldBe` p1Next
      (nextPeer p2) `shouldBe` p2Next
      (nextPeer p3) `shouldBe` p3Next
      (nextPeer p4) `shouldBe` p4And5Next
      (nextPeer p5) `shouldBe` p4And5Next

    it "should return the same peer ID given it's a cell start ID" $ do
      let p = PeerID 4 (BS.pack [0, 0, 0, 255])

      (nextPeer p) `shouldBe` p

    it "should return the highest cell peer ID if the given peer ID is higher" $ do
      let p = PeerID 4 (BS.pack [255, 0, 0, 1])
          pHighest = PeerID 4 (BS.pack [255, 0, 0, 0])

      (nextPeer p) `shouldBe` pHighest

  describe "previousPeer" $ do
    it "should return the cell peer ID preceding the given peer ID" $ do
      let p1 = PeerID 4 (BS.pack [0, 0, 0, 1])
          p1Prev = PeerID 4 (BS.pack [0, 0, 0, 0])
          p2 = PeerID 4 (BS.pack [0, 1, 0, 0])
          p2Prev = PeerID 4 (BS.pack [0, 0, 255, 0])
          p3 = PeerID 4 (BS.pack [1, 0, 0, 0])
          p4 = PeerID 4 (BS.pack [254, 255, 255, 255])
          p3And4Prev = PeerID 4 (BS.pack [0, 255, 0, 0])

      (previousPeer p1) `shouldBe` p1Prev
      (previousPeer p2) `shouldBe` p2Prev
      (previousPeer p3) `shouldBe` p3And4Prev
      (previousPeer p4) `shouldBe` p3And4Prev

    it "should return the same peer ID given it's a cell start ID" $ do
      let p1 = PeerID 4 (BS.pack [0, 0, 0, 255])
          p2 = PeerID 4 (BS.pack [0, 0, 0, 0])

      (previousPeer p1) `shouldBe` p1
      (previousPeer p2) `shouldBe` p2

  describe "findPeer" $ do
    it "should return a peer from the local routing table if the given target is in the routing table" $ do
      let routes = RoutingTable { tableSize = 4, tablePeers = ps }
          ps = [ (p1, "192.168.0.1", 1 :: PortNumber)
               , (p2, "192.168.0.2", 1 :: PortNumber)
               , (p3, "192.168.0.3", 1 :: PortNumber)
               , (p4, "192.168.0.4", 1 :: PortNumber)
               ]
          p1 = PeerID 4 (BS.pack [0,   0,   0,   0])
          p2 = PeerID 4 (BS.pack [0,   0,   255, 0])
          p3 = PeerID 4 (BS.pack [0,   255, 0,   0])
          p4 = PeerID 4 (BS.pack [255, 0,   0,   0])

          dummyMsgSender _ _ = return $ FindPeerFail "dummy"

      findP3 <- findPeer dummyMsgSender routes p3

      findP3 `shouldBe` p3

    it "should send the FindPeer message to the closest peer to the given target in the routing table" $ do
      let routes = RoutingTable { tableSize = 4, tablePeers = ps }
          ps = [ (p1, "192.168.0.1", 1 :: PortNumber)
               , (p2, "192.168.0.2", 1 :: PortNumber)
               , (p3, "192.168.0.3", 1 :: PortNumber)
               , (p4, "192.168.0.4", 1 :: PortNumber)
               ]
          p1 = PeerID 4 (BS.pack [0,   0,   0,   0])
          p2 = PeerID 4 (BS.pack [0,   0,   255, 0])
          p3 = PeerID 4 (BS.pack [0,   255, 0,   0])
          p4 = PeerID 4 (BS.pack [255, 0,   0,   0])

          p5 = PeerID 4 (BS.pack [255, 0,   0,   1])

          mockMsgSender (FindPeer findId) targetId
            | findId == p5 && targetId == p4 = return $ FindPeerR p5
            | findId == p5 && targetId /= p4 = fail "FindPeer message sent to wrong target"
            | findId /= p5 = fail "FindPeer message sent with wrong search peer ID"
          mockMsgSender _ _ = fail "Wrong message type sent to message sender"

      findP5 <- findPeer mockMsgSender routes p5

      findP5 `shouldBe` p5

    it "should accept the closest found peer ID from the network if no exact match is found" $ do
      let routes = RoutingTable { tableSize = 4, tablePeers = ps }
          ps = [ (p1, "192.168.0.1", 1 :: PortNumber)
               , (p2, "192.168.0.2", 1 :: PortNumber)
               , (p3, "192.168.0.3", 1 :: PortNumber)
               , (p4, "192.168.0.4", 1 :: PortNumber)
               ]
          p1 = PeerID 4 (BS.pack [0,   0,   0,   0])
          p2 = PeerID 4 (BS.pack [0,   0,   255, 0])
          p3 = PeerID 4 (BS.pack [0,   255, 0,   0])
          p4 = PeerID 4 (BS.pack [255, 0,   0,   0])

          p5 = PeerID 4 (BS.pack [255, 0,   0,   1])
          p6 = PeerID 4 (BS.pack [255, 0,   0,   2])

          mockMsgSender (FindPeer findId) targetId
            | findId == p5 && targetId == p4 = return $ FindPeerR p6
            | findId == p5 && targetId == p6 = return $ FindPeerR p6
            | findId /= p5 = fail "FindPeer message sent with wrong search peer ID"
          mockMsgSender _ _ = fail "Wrong message type sent to message sender"

      findP5 <- findPeer mockMsgSender routes p5

      findP5 `shouldBe` p6

fakeTicket :: Ticket
fakeTicket = Ticket
  { ticketId = "1"
  , ticketDescription = "Test"
  , ticketFaceValue = 1
  }

fakeHolder :: Holder
fakeHolder = Holder
  { holderIdentity = "holder"
  , holderPublicKey = "key"
  , holderFingerprint = "finger-print"
  }

fakeUTCTime :: UTCTime
fakeUTCTime = UTCTime
  { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
  , utctDayTime = secondsToDiffTime 0
  }

fakeTransaction :: Ticket -> Maybe Holder -> Holder -> UTCTime -> Transaction
fakeTransaction t o d ts = hashTransaction
  Transaction
  { transId = "1"
  , transTicket = t
  , transOrigin = o
  , transDestination = d
  , transTimestamp = (formatTime defaultTimeLocale rfc822DateFormat ts)
  , transValue = 0
  , transOriginSignature = ""
  , transDestinationSignature = ""
  , transPreceding = Nothing
  , transHash = ""
  }

shouldBeRight :: (HasCallStack, Show a, Show b) => Either a b -> Expectation
shouldBeRight (Left a) = assertFailure $ "Did not expect Left value: " ++ show a
shouldBeRight (Right _) = return ()

shouldNotBeLeft :: (HasCallStack, Show a, Show b) => Either a b -> Expectation
shouldNotBeLeft = shouldBeRight

shouldBeLeft :: (HasCallStack, Show a, Show b) => Either a b -> Expectation
shouldBeLeft (Right a) = assertFailure $ "Did not expect Right value: " ++ show a
shouldBeLeft (Left _) = return ()

shouldNotBeRight :: (HasCallStack, Show a, Show b) => Either a b -> Expectation
shouldNotBeRight = shouldBeLeft

leftShouldBe :: (HasCallStack, Show a, Show b, Eq a) => Either a b -> a -> Expectation
leftShouldBe (Left a) b = a `shouldBe` b
leftShouldbe (Right a) _ = assertFailure $ "Did not expect Right value: " ++ show a

rightShouldBe :: (HasCallStack, Show a, Show b, Eq b) => Either a b -> b -> Expectation
rightShouldBe (Left a) _ = assertFailure $ "Did not expect Left value: " ++ show a
rightShouldBe (Right a) b = a `shouldBe` b

leftShouldSatisfy :: (HasCallStack, Show a, Show b) => Either a b -> (a -> Bool) -> Expectation
leftShouldSatisfy (Left a) p = a `shouldSatisfy` p
leftShouldSatisfy (Right a) _ = assertFailure $ "Did not expect Right value: " ++ show a

rightShouldSatisfy :: (HasCallStack, Show a, Show b) => Either a b -> (b -> Bool) -> Expectation
rightShouldSatisfy (Right a) p = a `shouldSatisfy` p
rightShouldSatisfy (Left a) _ = assertFailure $ "Did not expect Left value: " ++ show a
