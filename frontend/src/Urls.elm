module Urls exposing (..)


allDesignStubs : String
allDesignStubs =
    "https://pragmaticproteindesign.bio.ed.ac.uk/pda/all-design-stubs"


designDetailsFromId : String -> String
designDetailsFromId designId =
    "https://pragmaticproteindesign.bio.ed.ac.uk/pda/design-details/" ++ designId
