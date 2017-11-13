module TicketChainSpec
  (spec)
where

import Data.Time.Calendar
import Data.Time.Clock
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
  , transTimestamp = ts
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
