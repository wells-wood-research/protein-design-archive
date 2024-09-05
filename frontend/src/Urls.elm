module Urls exposing (..)

import Json.Encode as Encode


downloadSelectedDesigns : List String -> String
downloadSelectedDesigns pdbCodes =
    if List.isEmpty pdbCodes then
        "https://pragmaticproteindesign.bio.ed.ac.uk/pda-api/all-designs"

    else
        let
            encodedPdbCodes =
                Encode.encode 0 (Encode.list Encode.string pdbCodes)
        in
        "https://pragmaticproteindesign.bio.ed.ac.uk/pda-api/all-designs?pdb-codes=" ++ encodedPdbCodes


allDesignStubs : String
allDesignStubs =
    "https://pragmaticproteindesign.bio.ed.ac.uk/pda-api/all-design-stubs"


designDetailsFromId : String -> String
designDetailsFromId designId =
    "https://pragmaticproteindesign.bio.ed.ac.uk/pda-api/design-details/" ++ designId
