module EVE(
         -- * State initialization
         EVE, Offline, Online, Keyed
       , initializeEVEOffline
       , initializeEVEOnline
       , initializeEVEKeyed
       , addUserInfo
       , removeUserInfo
       , goOffline
       )
 where

import EVE.State
