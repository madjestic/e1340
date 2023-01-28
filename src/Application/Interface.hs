{-# LANGUAGE TemplateHaskell #-}

module Application.Interface
  where

--import Control.Lens ( view, makeLenses )

data Planet =
    None
  | Earth
  deriving Show

-- data Interface =
--     IntrApp { _inpQuit :: Bool
--             , _inpOpts :: Bool }
--   | OptsApp { _inpBack :: Bool }
--   | InfoApp Planet
--   | MainApp 
--   | Finished
--   deriving Show
-- $(makeLenses ''Interface)
