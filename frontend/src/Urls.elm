module Urls exposing (..)

import Json.Encode as Encode


downloadSelectedDesigns : List String -> String
downloadSelectedDesigns pdbCodes =
    if List.isEmpty pdbCodes then
        "http://localhost:5000/all-designs"

    else
        let
            encodedPdbCodes =
                Encode.encode 0 (Encode.list Encode.string pdbCodes)
        in
        "http://localhost:5000/all-designs?pdb-codes=" ++ encodedPdbCodes


allDesignStubs : String
allDesignStubs =
    "http://localhost:5000/all-design-stubs"


designDetailsFromId : String -> String
designDetailsFromId designId =
    "http://localhost:5000/design-details/" ++ designId
