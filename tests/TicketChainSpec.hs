module TicketChainSpec
  (spec)
where

import Data.Time.Calendar
import Data.Time.Clock
import Test.Hspec
import TicketChain

spec :: Spec
spec = do
  describe "appendTransaction" $ do
    it "should update the head of the given chain" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder laterTime
          laterTime =
            UTCTime
            { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
            , utctDayTime = secondsToDiffTime 60
            }
          updatedChain =
            appendTransaction initialChain newTransaction
      (transTimestamp $ chainHead updatedChain) `shouldBe` transTimestamp newTransaction

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
          updatedChain =
            appendTicketTransfer initialChain newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
      (transTicket $ chainHead updatedChain) `shouldBe` newTicket
      (transValue $ chainHead updatedChain) `shouldBe` newValue

    it "should sign the transaction with the origin holder key" $ do
      let initialChain =
            Chain { chainHead = existingTransaction }
          existingTransaction =
            fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
          newTicket =
            fakeTicket { ticketId = 2 }
          origin =
            Just fakeHolder
            { holderIdentity = "origin"
            , holderPublicKey = "origin-pub-key"
            }
          originKey =
            "origin-priv-key"
          ticketDigest =
            signString (serialiseTransaction $ chainHead updatedChain) originKey
          updatedChain =
            appendTicketTransfer initialChain newTicket 0 origin originKey fakeHolder "key" fakeUTCTime
      (transOriginSignature $ chainHead updatedChain) `shouldBe` ticketDigest

    it "should sign the transaction with the destination holder key" $ do
      pending
    it "should update the head of the given chain" $ do
      pending
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
  { transTicket = t
  , transOrigin = o
  , transDestination = d
  , transTimestamp = ts
  , transValue = 0
  , transOriginSignature = ""
  , transDestinationSignature = ""
  , transPreceding = Nothing
  , transFollowing = Nothing
  , transHash = ""
  }
