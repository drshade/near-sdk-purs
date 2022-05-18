module Test.Records where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row (class Union)
import Record as Record

type Dog = { dog :: String }
type Cat = { cat :: String }

type DogCat = { dog :: String, cat :: String }

mm :: forall (d :: Row Type) (c :: Row Type) (e :: Row Type). Union d c e => Record d -> Record c -> Record e
mm dog cat = Record.union dog cat

main :: Effect Unit
main = 
    let a :: Dog
        a = { dog : "dog" }
        b :: Cat
        b = { cat : "cat" } 
        c :: DogCat
        c = Record.union a b
    in
    log $ show $ Record.merge a b