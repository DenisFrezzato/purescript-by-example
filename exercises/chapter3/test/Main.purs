module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry, removeDuplicates, isInAddressBook, findEntryByAddress)
import Data.Maybe (Maybe)

example :: Entry
example = {
  firstName: "John",
  lastName: "Smith",
  address: {
    street: "123 Fake St.",
    city: "Faketown",
    state: "CA"
  }
}

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main :: Effect Unit
main = do
  let book1 = insertEntry example emptyBook

  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1

  logShow $ findEntryByAddress example.address.street book1

  logShow $ isInAddressBook "John" "Smith" book1

  let book2 = insertEntry example book1

  logShow $ removeDuplicates book2
