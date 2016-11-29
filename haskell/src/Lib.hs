module Lib where

  type Name = String

  data Sex = Male | Female

  instance Show Sex where
     show (Male) = "a male"
     show (Female) = "a female"

  data Patient = Patient { name :: Name, sex :: Sex, age :: Int }

  run = do
    let johnDoe = Patient {
      name = "Johannes Link",
      sex = Male,
      age = 30
    }
    putStrLn $ "Name: " ++ name johnDoe
    putStrLn $ "Age: " ++ show(age johnDoe)
    putStrLn $ "Sex: " ++ show(sex johnDoe)
    return ()
