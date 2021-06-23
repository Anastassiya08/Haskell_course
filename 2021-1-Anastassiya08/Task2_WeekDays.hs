module Task2_WeekDays where

import           Test.Tasty.HUnit
import           Test.QuickCheck

data WeekDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving Show

instance Arbitrary WeekDay where
  arbitrary = elements [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

instance Eq WeekDay where
  Sun == Sun = True 
  Mon == Mon = True  
  Tue == Tue = True  
  Wed == Wed = True
  Thu == Thu = True  
  Fri == Fri = True  
  Sat == Sat = True    
  _ == _ = False 

n_of_day :: WeekDay -> Integer
n_of_day day = case day of Sun -> 0
                           Mon -> 1
                           Tue -> 2
                           Wed -> 3
                           Thu -> 4
                           Fri -> 5
                           Sat -> 6
                           
n_to_day :: Integer -> WeekDay
n_to_day n = case n of 0 -> Sun
                       1 -> Mon
                       2 -> Tue
                       3 -> Wed
                       4 -> Thu
                       5 -> Fri
                       6 -> Sat


nextDay :: WeekDay -> WeekDay
nextDay day = n_to_day (mod (n_of_day day + 1) 7)

unit_nextDay_Mon :: Assertion
unit_nextDay_Mon = nextDay Mon @?= Tue

unit_nextDay_Tue :: Assertion
unit_nextDay_Tue = nextDay Tue @?= Wed

unit_nextDay_Wed :: Assertion
unit_nextDay_Wed = nextDay Wed @?= Thu

unit_nextDay_Thu :: Assertion
unit_nextDay_Thu = nextDay Thu @?= Fri

unit_nextDay_Fri :: Assertion
unit_nextDay_Fri = nextDay Fri @?= Sat

unit_nextDay_Sat :: Assertion
unit_nextDay_Sat = nextDay Sat @?= Sun

unit_nextDay_Sun :: Assertion
unit_nextDay_Sun = nextDay Sun @?= Mon

afterDays :: Integer -> WeekDay -> WeekDay
afterDays n day = n_to_day (mod (n_of_day day + n) 7)

prop_afterDays_7 :: Integer -> WeekDay -> Property
prop_afterDays_7 i d = afterDays (i * 7) d === d

isWeekend :: WeekDay -> Bool
isWeekend day = if day == Sat || day == Sun
                  then True
                  else False
                  
unit_isWeekend_Sun :: Assertion
unit_isWeekend_Sun = isWeekend Sun @?= True                  

unit_isWeekend_Wed :: Assertion
unit_isWeekend_Wed = isWeekend Wed @?= False

daysToParty :: WeekDay -> Integer
daysToParty day = if day == Sat
                    then 6
                    else 5 - (n_of_day day)

unit_daysToParty_Sun :: Assertion
unit_daysToParty_Sun = daysToParty Sun @?= 5                                  
                          
unit_daysToParty_Mon :: Assertion
unit_daysToParty_Mon = daysToParty Mon @?= 4

unit_daysToParty_Wed :: Assertion
unit_daysToParty_Wed = daysToParty Wed @?= 2

unit_daysToParty_Fri :: Assertion
unit_daysToParty_Fri = daysToParty Fri @?= 0

unit_daysToParty_Sat :: Assertion
unit_daysToParty_Sat = daysToParty Sat @?= 6


