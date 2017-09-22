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
      let chain =
            appendTransaction Chain { chainHead = trans1 } trans2
          trans1 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "first" }
          trans2 = (fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime) { transId = "second" }
      chain `rightShouldSatisfy` (\c -> length (traverseChain id c) == 2)

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
            { ticketId = 2
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          updatedChain =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      updatedChain `rightShouldSatisfy` (\c -> (transTicket $ chainHead c) == newTicket)
      updatedChain `rightShouldSatisfy` (\c -> (transValue $ chainHead c) == newValue)

    it "should sign the transaction with the origin holder key" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            fakeTicket { ticketId = 2 }
          newTransId = "new-trans-id"
          origin =
            Just fakeHolder
            { holderIdentity = "origin"
            , holderPublicKey = "origin-pub-key"
            }
          originKey =
            "origin-priv-key"
          ticketDigest c =
            signString (serialiseTransaction $ chainHead c) originKey
          updatedChain =
            appendTicketTransfer initialChain newTransId newTicket 0 origin originKey fakeHolder "key" fakeUTCTime
      updatedChain `rightShouldSatisfy` (\c -> (transOriginSignature $ chainHead c) == ticketDigest c)

    it "should sign the transaction with the destination holder key" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            fakeTicket { ticketId = 2 }
          newTransId = "new-trans-id"
          destination =
            fakeHolder
            { holderIdentity = "destination"
            , holderPublicKey = "destination-pub-key"
            }
          destinationKey =
            "destination-priv-key"
          ticketDigest c =
            signString (serialiseTransaction $ chainHead c) destinationKey
          updatedChain =
            appendTicketTransfer initialChain newTransId newTicket 0 Nothing "" destination destinationKey fakeUTCTime
      updatedChain `rightShouldSatisfy` (\c -> (transDestinationSignature $ chainHead c) == ticketDigest c)

    it "should update the head of the given chain" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            Ticket
            { ticketId = 2
            , ticketDescription = "New ticket"
            , ticketFaceValue = newValue
            }
          newValue = 5
          newTransId = "new-trans-id"
          updatedChain =
            appendTicketTransfer initialChain newTransId newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      updatedChain `rightShouldSatisfy` (\c -> (chainHead c) /= existingTransaction)

    it "should leave only one transaction for the ticket given there is no origin holder" $ do
      pending

fakeTicket :: Ticket
fakeTicket = Ticket
  { ticketId = 1
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
fakeTransaction t o d ts = Transaction
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
