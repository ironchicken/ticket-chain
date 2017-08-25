module TicketChainTests where

import Data.Time.Calendar
import Data.Time.Clock
import Distribution.TestSuite
import TicketChain

mkTestInstance :: String -> IO Progress -> TestInstance
mkTestInstance description runTest = TestInstance
  { run = runTest
  , name = description
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkTestInstance description runTest
  }

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
appendTransaction_should_update_the_head_of_the_given_chain =
  mkTestInstance "appendTransaction should update the head of the given chain" runTest
  where
    runTest =
      return $ checkUpdatedChain updatedChain newTransaction
    checkUpdatedChain c t =
      Finished $ if verifyChainHead c t == True
                 then Pass
                 else Fail $ "chainHead is " ++ (show $ chainHead c) ++ "; expecting: " ++ (show t)
    verifyChainHead c t =
      (transTimestamp $ chainHead c) == transTimestamp t

    updatedChain =
      appendTransaction initialChain newTransaction
    initialChain =
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

appendTicketTransfer_should_create_a_new_transaction :: TestInstance
appendTicketTransfer_should_create_a_new_transaction =
  mkTestInstance "appendTicketTransfer should create a new transaction" runTest
  where
    runTest =
      return $ checkUpdatedChain updatedChain newTicket newValue
    checkUpdatedChain chain ticket value =
      Finished $ if verifyChainHead chain ticket value == True
                 then Pass
                 else Fail $ "chainHead ticket is " ++ (show $ transTicket $ chainHead chain) ++ "; expecting: " ++ (show ticket)
    verifyChainHead chain ticket value =
      (transTicket $ chainHead chain) == ticket
      && (transValue $ chainHead chain) == value

    updatedChain =
      appendTicketTransfer initialChain newTicket newValue Nothing "" fakeHolder "key" fakeUTCTime
    initialChain =
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

appendTicketTransfer_should_sign_the_transaction_with_the_origin_holder_key :: TestInstance
appendTicketTransfer_should_sign_the_transaction_with_the_origin_holder_key =
  mkTestInstance "appendTicketTransfer should sign the transaction with the origin holder key" runTest
  where
    runTest =
      return $ checkUpdatedChain updatedChain ticketDigest
    checkUpdatedChain chain signature =
      Finished $ if verifyChainHead chain signature == True
                 then Pass
                 else Fail $ "chainHead transaction origin signature is " ++ (show $ transOriginSignature $ chainHead chain) ++ "; expecting: " ++ (show signature)
    verifyChainHead chain signature =
      (transOriginSignature $ chainHead chain) == signature

    updatedChain =
      appendTicketTransfer initialChain newTicket 0 origin originKey fakeHolder "key" fakeUTCTime
    initialChain =
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

appendTicketTransfer_should_sign_the_transaction_with_the_destination_holder_key :: TestInstance
appendTicketTransfer_should_sign_the_transaction_with_the_destination_holder_key =
  mkTestInstance "appendTicketTransfer should sign the transaction with the destination holder key" runTest
  where
    runTest = return $ Finished $ Fail "todo"

appendTicketTransfer_should_update_the_head_of_the_given_chain :: TestInstance
appendTicketTransfer_should_update_the_head_of_the_given_chain =
  mkTestInstance "appendTicketTransfer should update the head of the given chain" runTest
  where
    runTest = return $ Finished $ Fail "todo"

appendTicketTransfer_should_leave_only_one_transaction_for_the_ticket_given_there_is_no_origin_holder :: TestInstance
appendTicketTransfer_should_leave_only_one_transaction_for_the_ticket_given_there_is_no_origin_holder =
  mkTestInstance "appendTicketTransfer should leave only one transaction for the ticket given there is no origin holder" runTest
  where
    runTest = return $ Finished $ Fail "todo"

tests :: IO [Test]
tests = return
  [ Test $ appendTransaction_should_update_the_head_of_the_given_chain
  , Test $ appendTicketTransfer_should_create_a_new_transaction
  , Test $ appendTicketTransfer_should_sign_the_transaction_with_the_origin_holder_key
  , Test $ appendTicketTransfer_should_sign_the_transaction_with_the_destination_holder_key
  , Test $ appendTicketTransfer_should_update_the_head_of_the_given_chain
  , Test $ appendTicketTransfer_should_leave_only_one_transaction_for_the_ticket_given_there_is_no_origin_holder
  ]
