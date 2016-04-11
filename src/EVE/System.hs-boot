module EVE.System(SolarSystemId, toSolarSystemId) where

import EVE.State
import EVE.Static.Database.TypeIds

newtype SolarSystemId = SSID { _ssid :: TypeId }

instance Eq SolarSystemId
instance Ord SolarSystemId
instance Read SolarSystemId
instance Show SolarSystemId

toSolarSystemId :: EVE a -> TypeId -> Maybe SolarSystemId
