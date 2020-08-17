module Asana.Api.Generic (GenericAsanaTask) where

import Data.Aeson (FromJSON)

-- Class of things that can be parsed as "tasks"
--
-- Since Asana's API is extensible, it makes sense to allow the user some
-- ability to change the way a @Task@ is parsed. This gives us a little bit of
-- flexibility when it comes to the myriad ways a @Task@ list can be fetched
-- (see @getProjectTasks@)
class FromJSON a => GenericAsanaTask a
