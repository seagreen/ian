module Goal where

import Data.Text
import Data.Time
import Prelude
import TinyServant

type ExampleAPI =
       "date" :> Get Day
  :<|> "uppercase" :> Capture Text :>
                        (    Get Text
                        :<|> "and-append-number" :> Capture Int :> Get Text
                        )

-- | In the real thing Example will also have type parameters
-- a request body, URL query parameters, etc,
-- but we don't need to worry about that yet.
data Example capture output = Example capture output

exampleDocs
  :: [Example () Day] :<|> [Example Text Text] :<|> [Example (Text, Int) Text]
exampleDocs =
       [Example () (fromGregorian 2000 1 1)]
  :<|> [Example "foo" "FOO", Example "bar" "BAR"]
  :<|> [Example ("baz", 100) "BAZ100"]

finalOutput :: Text
finalOutput =
  undefined -- The goal is:
            --
            --     document (Proxy @ExampleAPI) exampleDocs

class HasDocs api where
  type Doc api :: *

  document :: Proxy api -> Doc api -> Text
