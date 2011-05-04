module EVE.Internal.DB.Tables(
         requestCache, cacheRequestCol, cacheResponseCol, cacheExpireCol
       )
 where

import Database.SQLite

-- |The request cache table in our database. Contains three columns: a hash
-- of the request made, a time at which we should expire the request, and
-- the result of the request.
requestCache :: SQLTable
requestCache = Table {
    tabName = "RequestCache"
  , tabConstraints = [TablePrimaryKey ["request"]]
  , tabColumns = [
      Column { colName = cacheRequestCol, colType = SQLChar Nothing,
               colClauses = [PrimaryKey False, Unique, IsNullable False] }
    , Column { colName = cacheExpireCol, colType = SQLDateTime DATETIME,
               colClauses = [IsNullable False] }
    , Column { colName = cacheResponseCol, colType = SQLBlob LongBlob,
               colClauses = [IsNullable False] }
    ]
  }

cacheRequestCol, cacheResponseCol, cacheExpireCol :: String
cacheRequestCol  = "request"
cacheResponseCol = "response"
cacheExpireCol   = "expire"
