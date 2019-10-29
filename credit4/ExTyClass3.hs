import Data.Text 
class JSON a where
  definitions

toJSONString :: JSON a => a -> String
