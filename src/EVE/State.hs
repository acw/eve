{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
module EVE.State(
         EVE
       , Offline, Online, Keyed
       , IsOnline, stMapAPI
       , stDatabase
       , stUserInfo
       , initializeEVEOffline
       , initializeEVEOnline
       , initializeEVEKeyed
       , addUserInfo, removeUserInfo, goOffline
       )
 where

import Control.Lens(Lens', view, set)
import Control.Lens.TH(makeLenses)
import EVE.API.XML.Map(MapAPI, initMapAPI)
import EVE.Console(Console)
import EVE.Static.Database(EVEDB, loadStaticData)
import EVE.UserInfo(UserInfo)

data EVE state = ES {
       _estDatabase :: EVEDB
     , _estState    :: state
     }
makeLenses ''EVE

stDatabase :: Lens' (EVE a) EVEDB
stDatabase = estDatabase

class IsOnline a where
  stMapAPI :: Lens' (EVE a) MapAPI

newtype Offline = Offline ()

data Online  = Online {
       _estMapAPI :: MapAPI
     }
makeLenses ''Online

instance IsOnline Online where
  stMapAPI = estState . estMapAPI

data Keyed   = Keyed {
       _estOnline   :: Online
     , _estUserInfo :: UserInfo
     }
makeLenses ''Keyed

instance IsOnline Keyed where
  stMapAPI = estState . estOnline . estMapAPI

initializeEVEOffline :: Console -> IO (EVE Offline)
initializeEVEOffline con =
  do _estDatabase <- loadStaticData con
     let _estState = Offline ()
     return ES{..}

initializeEVEOnline :: Console -> IO (EVE Online)
initializeEVEOnline con =
  do _estMapAPI   <- initMapAPI     con
     _estDatabase <- loadStaticData con
     let _estState = Online {..}
     return ES{..}

initializeEVEKeyed :: Console -> UserInfo -> IO (EVE Keyed)
initializeEVEKeyed con userinfo =
  addUserInfo userinfo `fmap` initializeEVEOnline con

addUserInfo :: UserInfo -> EVE Online -> EVE Keyed
addUserInfo uinfo state = set estState Keyed{..} state
 where
  _estOnline   = view estState state
  _estUserInfo = uinfo

removeUserInfo :: EVE Keyed -> EVE Online
removeUserInfo state = set estState online state
 where online = view estOnline (view estState state)

goOffline :: EVE a -> EVE Offline
goOffline = set estState (Offline ())

stUserInfo :: Lens' (EVE Keyed) UserInfo
stUserInfo = estState . estUserInfo
