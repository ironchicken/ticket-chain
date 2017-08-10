module TicketChainTests where

import Data.Time.Calendar
import Data.Time.Clock
import Distribution.TestSuite
import TicketChain

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

appendTransaction_should_update_the_head_of_the_given_chain :: TestInstance
appendTransaction_should_update_the_head_of_the_given_chain = TestInstance
  { run = return $ checkUpdatedChain updatedChain newTransaction
  , name = "appendTransaction should update the head of the given chain"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right appendTransaction_should_update_the_head_of_the_given_chain
  }
  where
    updatedChain = appendTransaction initialChain newTransaction
    checkUpdatedChain c t =
      Finished $ if verifyChainHead c t == True
                 then Pass
                 else Fail $ "chainHead is " ++ (show $ chainHead c) ++ "; expecting: " ++ (show t)
    initialChain = Chain { chainHead = existingTransaction }
    newTransaction = fakeTransaction fakeTicket Nothing fakeHolder laterTime
    laterTime = UTCTime
      { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
      , utctDayTime = secondsToDiffTime 60
      }
    existingTransaction = fakeTransaction fakeTicket Nothing fakeHolder fakeUTCTime
    verifyChainHead c t = (transTimestamp $ chainHead c) == transTimestamp t

tests :: IO [Test]
tests = return
  [ Test $ appendTransaction_should_update_the_head_of_the_given_chain ]
