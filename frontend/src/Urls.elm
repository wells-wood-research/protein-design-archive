module Urls exposing (..)


allDesignStubs : String
allDesignStubs =
    "http://localhost:5000/all-design-stubs"


designDetailsFromId : String -> String
designDetailsFromId designId =
    "http://localhost:5000/design-details/" ++ designId
