module HW1.T1 where

import Numeric.Natural (Natural)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

instance Show Day where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays 1 d = nextDay d
afterDays n d = afterDays (n - 1) (nextDay d)

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty d = 1 + daysToParty (nextDay d)
