module Blns exposing (blns)

import Base64

blns : List String
blns =
  [ ""
  ]
  |> List.map (Base64.decode >> Result.withDefault "")
