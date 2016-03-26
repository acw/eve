module EVE.System(SolarSystemId, toSolarSystemId) where

import EVE.Static.Database
import EVE.Static.Database.TypeIds

newtype SolarSystemId = SSID { _ssid :: TypeId }

instance Eq SolarSystemId
instance Ord SolarSystemId
instance Read SolarSystemId
instance Show SolarSystemId

toSolarSystemId :: EVEDB -> TypeId -> Maybe SolarSystemId
