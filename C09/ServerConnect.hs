{-# LANGUAGE MultiParamTypeClasses #-}

module ServerConnect where

class OSConfig where
  os_address :: String
  os_name :: String
  os_password :: String

connectToServer :: OSConfig => IO ()
connectToServer = do
  putStrLn $ "Connecting to " ++ os_address
  putStrLn $ "Sending name " ++ os_name
