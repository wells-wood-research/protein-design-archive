module Data exposing (Classification, Design, classificationToColour, classificationToString, getAllDesigns)

import Csv
import Date
import Time exposing (Month(..))



-- Things to add:
-- * Crystallisation conditions
-- * Temperature
-- * Publicatoin doi, link, authors and abstract
-- * All the pictures
--   * Have a toggle for asym unit and biol unit
--   * URLs are preferred for the pictures so we don't need to host
-- * NGL view?
-- * The design method
--;PDB ID;Dek's category;Classification;Date;Authors;DOI;Pubmed ID


type alias Design =
    { pdbCode : String
    , classification : Classification
    , structuralKeywords : String
    , depositionDate : Date.Date
    , picturePath : String

    -- , method : String
    , authors : String
    , doiLink : String

    -- , publicationTitle : String
    -- , sequences : List String
    }


type Classification
    = OriginalDeNovo
    | RelativeDeNovo
    | Small
    | Engineered
    | Unknown


rawStringToClassfication : String -> Classification
rawStringToClassfication string =
    case string of
        "original de novo design" ->
            OriginalDeNovo

        "relative of another de novo design" ->
            RelativeDeNovo

        "small, non-systematic, and other" ->
            Small

        "engineered" ->
            Engineered

        _ ->
            Unknown


classificationToString : Classification -> String
classificationToString classification =
    case classification of
        OriginalDeNovo ->
            "Original De Novo"

        RelativeDeNovo ->
            "Relative De Novo"

        Small ->
            "Small, Non-Systematic, Other"

        Engineered ->
            "Engineered"

        Unknown ->
            "Unknown"


classificationToColour : Classification -> String
classificationToColour classification =
    case classification of
        OriginalDeNovo ->
            "#ff0000"

        RelativeDeNovo ->
            "#00ff00"

        Small ->
            "#ffffff"

        Engineered ->
            "#0000ff"

        Unknown ->
            "#333333"


getAllDesigns : List Design
getAllDesigns =
    Csv.parseWith ";" rawData
        |> .records
        |> List.filterMap listStringsToDesign


listStringsToDesign : List String -> Maybe Design
listStringsToDesign strings =
    case strings of
        [ _, rPdb, rawClassification, keywords, date, authors, doi, _ ] ->
            let
                pdbCode =
                    String.toLower rPdb

                classification =
                    rawStringToClassfication rawClassification

                depositionDate =
                    stringToIsoDate date
                        |> Debug.log "date"
                        |> Date.fromIsoString
                        |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)

                picturePath =
                    "https://cdn.rcsb.org/images/structures/"
                        ++ String.slice 1 3 pdbCode
                        ++ "/"
                        ++ pdbCode
                        ++ "/"
                        ++ pdbCode
                        ++ "_assembly-1.jpeg"
            in
            Design pdbCode classification keywords depositionDate picturePath authors doi |> Just

        _ ->
            Nothing


stringToIsoDate : String -> String
stringToIsoDate date =
    case String.split "-" date of
        [ day, month, year ] ->
            let
                fixedYear =
                    String.toInt year
                        |> Maybe.withDefault 60
                        |> (\y ->
                                if y < 50 then
                                    "20" ++ (String.fromInt y |> yearStringTo2Char)

                                else
                                    "19" ++ (String.fromInt y |> yearStringTo2Char)
                           )
            in
            fixedYear ++ "-" ++ monthStringToNumberString month ++ "-" ++ day

        _ ->
            "1950-01-01"


yearStringTo2Char : String -> String
yearStringTo2Char year =
    case String.length year of
        2 ->
            year

        1 ->
            "0" ++ year

        _ ->
            "00"


monthStringToNumberString : String -> String
monthStringToNumberString month =
    case month of
        "JAN" ->
            "01"

        "FEB" ->
            "02"

        "MAR" ->
            "04"

        "APR" ->
            "04"

        "MAY" ->
            "05"

        "JUN" ->
            "06"

        "JUL" ->
            "07"

        "AUG" ->
            "08"

        "SEP" ->
            "09"

        "OCT" ->
            "10"

        "NOV" ->
            "11"

        "DEC" ->
            "12"

        _ ->
            "01"



-- getAllDesigns : List Design
-- getAllDesigns =
--     JDe.decodeString
--         (designDecoder
--             |> JDe.field "data"
--             |> JDe.list
--         )
--         rawData
--         |> Debug.log "eep"
--         |> Result.withDefault []
-- designDecoder : JDe.Decoder Design
-- designDecoder =
--     JDe.succeed makeDesign
--         |> required "rcsb_id" JDe.string
--         |> requiredAt [ "rcsb_primary_citation", "year" ] JDe.int
--         |> required "exptl" (JDe.index 0 (JDe.field "method" JDe.string))
--         |> requiredAt [ "struct_keywords", "pdbx_keywords" ] JDe.string
--         |> requiredAt [ "pubmed", "rcsb_pubmed_doi" ] JDe.string
--         |> requiredAt [ "rcsb_primary_citation", "title" ] JDe.string
--         |> requiredAt [ "rcsb_primary_citation", "rcsb_authors" ] (JDe.list JDe.string)
--         |> required "polymer_entities"
--             (JDe.list
--                 (JDe.at [ "entity_poly", "pdbx_seq_one_letter_code_can" ] JDe.string)
--             )
-- makeDesign : String -> Int -> String -> String -> String -> String -> List String -> List String -> Design
-- makeDesign inPdbCode depositionDate method structuralKeywords doi publicationTitle authors sequences =
--     let
--         pdbCode =
--             String.toLower inPdbCode
--     in
--     { pdbCode = pdbCode
--     , depositionDate =
--         String.fromInt depositionDate
--             ++ "-01-01"
--             |> Date.fromIsoString
--             |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)
--     , method = method
--     -- Example: https://cdn.rcsb.org/images/structures/pn/4pnb/4pnb_assembly-1.jpeg
--     , picturePath =
--         "https://cdn.rcsb.org/images/structures/"
--             ++ String.slice 1 3 pdbCode
--             ++ "/"
--             ++ pdbCode
--             ++ "/"
--             ++ pdbCode
--             ++ "_assembly-1.jpeg"
--     , structuralKeywords = structuralKeywords
--     , doi = doi
--     , publicationTitle = publicationTitle
--     , authors = authors
--     , sequences = sequences
--     }


rawData : String
rawData =
    """
;PDB ID;Dek's category;Classification;Date;Authors;DOI;Pubmed ID
0;1AL1;original de novo design;SYNTHETIC PROTEIN MODEL         ;02-JUL-90;C.P.HILL,D.H.ANDERSON,L.WESSON,W.F.DEGRADO,D.EISENBERG               ;http://dx.doi.org/10.1126/science.2382133;2382133.0
1;1HCW;original de novo design;GROWTH RESPONSE PROTEIN         ;20-SEP-96;B.IMPERIALI,M.STRUTHERS,R.P.CHENG                                    ;http://dx.doi.org/10.1126/science.271.5247.342;8553067.0
2;1BB1;original de novo design;DE NOVO PROTEIN DESIGN         ;28-APR-98;S.NAUTIYAL,T.ALBER                                                   ;http://dx.doi.org/10.1110/ps.8.1.84;10210186.0
3;1BYZ;relative of another de novo design;DE NOVO PROTEIN             ;20-OCT-98;G.G.PRIVE,D.H.ANDERSON,L.WESSON,D.CASCIO,D.EISENBERG                 ;http://dx.doi.org/10.1110/ps.8.7.1400;10422828.0
4;1COS;original de novo design;ALPHA-HELICAL BUNDLE          ;22-JAN-93;B.LOVEJOY,S.CHOE,D.CASCIO,D.K.MCRORIE,W.DEGRADO,D.EISENBERG          ;http://dx.doi.org/10.1126/science.8446897;8446897.0
5;1DJF;small, non-systematic, and other;DE NOVO PROTEIN             ;03-DEC-99;R.MONTSERRET,M.J.MCLEISH,A.BOCKMANN,C.GEOURJON,F.PENIN               ;http://dx.doi.org/10.1021/bi000208x;10913242.0
6;1EC5;original de novo design;DE NOVO PROTEIN             ;25-JAN-00;S.GEREMIA                                                            ;http://dx.doi.org/10.1073/pnas.97.12.6298;10841536.0
7;1FME;relative of another de novo design;DE NOVO PROTEIN             ;16-AUG-00;C.A.SARISKY,S.L.MAYO                                                 ;http://dx.doi.org/10.1006/jmbi.2000.4345;11292351.0
8;1FMH;original de novo design;TRANSCRIPTION              ;17-AUG-00;D.N.MARTI,I.JELESAROV,H.R.BOSSHARD                                   ;http://dx.doi.org/10.1021/bi001242e;11041845.0
9;1FSD;original de novo design;NOVEL SEQUENCE             ;09-JUN-97;B.I.DAHIYAT,S.L.MAYO                                                 ;http://dx.doi.org/10.1126/science.278.5335.82;9311930.0
10;1FSV;relative of another de novo design;BETA BETA ALPHA MOTIF          ;26-OCT-97;B.I.DAHIYAT,S.L.MAYO                                                 ;http://dx.doi.org/10.1126/science.278.5335.82;9311930.0
11;1G6U;original de novo design;DE NOVO PROTEIN             ;07-NOV-00;N.L.OGIHARA,G.GHIRLANDA,J.W.BRYSON,M.GINGERY,W.F.DEGRADO,D.EISENBERG ;http://dx.doi.org/10.1073/pnas.98.4.1404;11171963.0
12;1HQJ;small, non-systematic, and other;DE NOVO PROTEIN             ;18-DEC-00;P.BURKHARD,M.MEIER,A.LUSTIG                                          ;http://dx.doi.org/10.1110/ps.9.12.2294;11206050.0
13;1IC9;engineered;DE NOVO PROTEIN             ;30-MAR-01;J.J.OTTESEN,B.IMPERIALI                                              ;http://dx.doi.org/10.1038/88604;11373623.0
14;1ICL;engineered;DE NOVO PROTEIN             ;02-APR-01;J.J.OTTESEN,B.IMPERIALI                                              ;http://dx.doi.org/10.1038/88604;11373623.0
15;1ICO;engineered;DE NOVO PROTEIN             ;02-APR-01;J.J.OTTESEN,B.IMPERIALI                                              ;http://dx.doi.org/10.1038/88604;11373623.0
16;1J4M;engineered;DE NOVO PROTEIN             ;10-OCT-01;M.T.PASTOR,M.LOPEZ DE LA PAZ,E.LACROIX,L.SERRANO,E.PEREZ-PAYA        ;http://dx.doi.org/10.1073/pnas.012583999;11782528.0
17;1JM0;relative of another de novo design;DE NOVO PROTEIN             ;17-JUL-01;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1021/ja010506x;11749531.0
18;1JMB;relative of another de novo design;DE NOVO PROTEIN             ;18-JUL-01;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1021/ja010506x;11749531.0
19;1JY4;small, non-systematic, and other;DE NOVO PROTEIN             ;11-SEP-01;J.VENKATRAMAN,G.A.NAGANA GOWDA,P.BALARAM                             ;http://dx.doi.org/10.1021/ja0174276;11982362.0
20;1JY6;small, non-systematic, and other;DE NOVO PROTEIN             ;11-SEP-01;J.VENKATRAMAN,G.A.NAGANA GOWDA,P.BALARAM                             ;http://dx.doi.org/10.1021/ja0174276;11982362.0
21;1JY9;small, non-systematic, and other;DE NOVO PROTEIN             ;11-SEP-01;H.E.STANGER,F.A.SYUD,J.F.ESPINOSA,I.GIRIAT,T.MUIR,S.H.GELLMAN        ;http://dx.doi.org/10.1073/pnas.211536998;11593011.0
22;1K09;small, non-systematic, and other;DE NOVO PROTEIN             ;18-SEP-01;N.CARULLA,C.WOODWARD,G.BARANY                                        ;http://dx.doi.org/10.1110/ps.4440102;12021452.0
23;1K43;small, non-systematic, and other;DE NOVO PROTEIN             ;05-OCT-01;M.T.PASTOR,M.LOPEZ DE LA PAZ,E.LACROIX,L.SERRANO,E.PEREZ-PAYA        ;http://dx.doi.org/10.1073/pnas.012583999;11782528.0
24;1KD8;engineered;DE NOVO PROTEIN             ;12-NOV-01;A.E.KEATING,V.N.MALASHKEVICH,B.TIDOR,P.S.KIM                         ;http://dx.doi.org/10.1073/pnas.261563398;11752430.0
25;1KD9;engineered;DE NOVO PROTEIN             ;12-NOV-01;A.E.KEATING,V.N.MALASHKEVICH,B.TIDOR,P.S.KIM                         ;http://dx.doi.org/10.1073/pnas.261563398;11752430.0
26;1KDD;engineered;DE NOVO PROTEIN             ;12-NOV-01;A.E.KEATING,V.N.MALASHKEVICH,B.TIDOR,P.S.KIM                         ;http://dx.doi.org/10.1073/pnas.261563398;11752430.0
27;1KYC;small, non-systematic, and other;DE NOVO PROTEIN             ;04-FEB-02;P.BURKHARD,S.IVANINSKII,A.LUSTIG                                     ;http://dx.doi.org/10.1016/S0022-2836(02)00114-6;12054832.0
28;1L2Y;evolved;DE NOVO PROTEIN             ;25-FEB-02;J.W.NEIDIGH,R.M.FESINMEYER,N.H.ANDERSEN                              ;http://dx.doi.org/10.1038/nsb798;11979279.0
29;1L4X;small, non-systematic, and other;DE NOVO PROTEIN             ;06-MAR-02;M.MEIER,A.LUSTIG,U.AEBI,P.BURKHARD                                   ;http://dx.doi.org/10.1006/jsbi.2002.4467;12064934.0
30;1LE0;small, non-systematic, and other;DE NOVO PROTEIN             ;09-APR-02;A.G.COCHRAN,N.J.SKELTON,M.A.STAROVASNIK                              ;http://dx.doi.org/10.1073/pnas.091100898;11331745.0
31;1LE1;small, non-systematic, and other;DE NOVO PROTEIN             ;09-APR-02;A.G.COCHRAN,N.J.SKELTON,M.A.STAROVASNIK                              ;http://dx.doi.org/10.1073/pnas.091100898;11331745.0
32;1LE3;small, non-systematic, and other;PROTEIN BINDING             ;09-APR-02;A.G.COCHRAN,N.J.SKELTON,M.A.STAROVASNIK                              ;http://dx.doi.org/10.1073/pnas.091100898;11331745.0
33;1LQ7;original de novo design;DE NOVO PROTEIN             ;09-MAY-02;Q.-H.DAI,C.TOMMOS,E.J.FUENTES,M.BLOMBERG,P.L.DUTTON,A.J.WAND         ;http://dx.doi.org/10.1021/ja0264201;12224922.0
34;1LT1;relative of another de novo design;DE NOVO PROTEIN             ;20-MAY-02;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1002/anie.200390127;12569505.0
35;1M3W;original de novo design;DE NOVO PROTEIN             ;01-JUL-02;S.S.HUANG,B.R.GIBNEY,S.E.STAYROOK,P.L.DUTTON,M.LEWIS                 ;http://dx.doi.org/10.1016/s0022-2836(02)01441-9;12589764.0
36;1N0Q;relative of another de novo design;STRUCTURAL PROTEIN           ;14-OCT-02;L.K.MOSAVI,D.L.MINOR JR.,Z.-Y.PENG                                   ;http://dx.doi.org/10.1073/pnas.252537899;12461176.0
37;1N0R;original de novo design;STRUCTURAL PROTEIN           ;14-OCT-02;L.K.MOSAVI,D.L.MINOR JR.,Z.-Y.PENG                                   ;http://dx.doi.org/10.1073/pnas.252537899;12461176.0
38;1MJ0;original de novo design;DE NOVO PROTEIN             ;26-AUG-02;A.KOHL,H.K.BINZ,P.FORRER,M.T.STUMPP,A.PLUECKTHUN,                     M.G.GRUETTER                                                        ;http://dx.doi.org/10.1073/pnas.0337680100;12566564.0
39;1NA3;relative of another de novo design;DE NOVO PROTEIN             ;26-NOV-02;E.MAIN,Y.XIONG,M.COCCO,L.D'ANDREA,L.REGAN                            ;http://dx.doi.org/10.1016/s0969-2126(03)00076-5;12737816.0
40;1NA0;original de novo design;DE NOVO PROTEIN             ;26-NOV-02;E.MAIN,Y.XIONG,M.COCCO,L.D'ANDREA,L.REGAN                            ;http://dx.doi.org/10.1016/s0969-2126(03)00076-5;12737816.0
41;1N09;small, non-systematic, and other;DE NOVO PROTEIN             ;11-OCT-02;S.J.RUSSELL,T.BLANDL,N.J.SKELTON,A.G.COCHRAN                         ;http://dx.doi.org/10.1021/ja028075l;12517150.0
42;1N0A;small, non-systematic, and other;DE NOVO PROTEIN             ;11-OCT-02;T.BLANDL,A.G.COCHRAN,N.J.SKELTON                                     ;http://dx.doi.org/10.1110/ps.0228603;12538887.0
43;1N0C;small, non-systematic, and other;DE NOVO PROTEIN             ;11-OCT-02;S.J.RUSSEL,T.BLANDL,N.J.SKELTON,A.G.COCHRAN                          ;http://dx.doi.org/10.1021/ja028075l;12517150.0
44;1N0D;small, non-systematic, and other;DE NOVO PROTEIN             ;11-OCT-02;S.J.RUSSELL,T.BLANDL,N.J.SKELTON,A.G.COCHRAN                         ;http://dx.doi.org/10.1021/ja028075l;12517150.0
45;1NVO;relative of another de novo design;UNKNOWN FUNCTION            ;04-FEB-03;O.MAGLIO,F.NASTRI,V.PAVONE,A.LOMBARDI,W.F.DEGRADO                    ;http://dx.doi.org/10.1073/pnas.0730771100;12655072.0
46;1OVR;relative of another de novo design;DE NOVO PROTEIN             ;27-MAR-03;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1021/ja054199x;16332076.0
47;1OVU;relative of another de novo design;DE NOVO PROTEIN             ;27-MAR-03;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1021/ja054199x;16332076.0
48;1OVV;relative of another de novo design;DE NOVO PROTEIN             ;27-MAR-03;L.DI COSTANZO,S.GEREMIA                                              ;http://dx.doi.org/10.1021/ja054199x;16332076.0
49;1P68;original de novo design;DE NOVO PROTEIN             ;29-APR-03;Y.WEI,S.KIM,D.FELA,J.BAUM,M.H.HECHT                                  ;http://dx.doi.org/10.1073/pnas.1835644100;14593201.0
50;1PBZ;small, non-systematic, and other;DE NOVO PROTEIN             ;15-MAY-03;J.WANG,M.M.ROSENBLATT,K.S.SUSLICK                                    ;http://dx.doi.org/10.1073/pnas.2231273100;14595023.0
51;1PSV;engineered;DESIGNED PEPTIDE            ;29-OCT-97;B.I.DAHIYAT,C.A.SARISKY,S.L.MAYO                                     ;http://dx.doi.org/10.1006/jmbi.1997.1341;9367772.0
52;1PYZ;engineered;METAL BINDING PROTEIN          ;09-JUL-03;L.DI COSTANZO,S.GEREMIA,L.RANDACCIO,F.NASTRI,O.MAGLIO,A.LOMBARDI,     V.PAVONE                                                            ;http://dx.doi.org/10.1007/s00775-004-0600-x;15551102.0
53;1QP6;original de novo design;DE NOVO PROTEIN             ;01-JUN-99;R.B.HILL,W.F.DEGRADO                                                 ;http://dx.doi.org/10.1021/ja9733649;
54;1QYS;original de novo design;DE NOVO PROTEIN             ;11-SEP-03;B.KUHLMAN,G.DANTAS,G.C.IRETON,G.VARANI,B.L.STODDARD,D.BAKER          ;http://dx.doi.org/10.1126/science.1089427;14631033.0
55;2MBM;original de novo design;DE NOVO PROTEIN             ;02-AUG-13;G.LIU,A.L.ZANGHELLINI,K.CHAN,R.XIAO,H.JANJUA,S.KOGAN,M.MAGLAQUI,      C.CICCOSANTI,T.B.ACTON,G.KORNHABER,J.K.EVERETT,D.BAKER,              G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)      ;;
56;1RH4;original de novo design;COILED COIL               ;07-OCT-98;P.B.HARBURY,J.J.PLECS,B.TIDOR,T.ALBER,P.S.KIM                        ;http://dx.doi.org/10.1126/science.282.5393.1462;9822371.0
57;1S9Z;original de novo design;DE NOVO PROTEIN             ;06-FEB-04;R.A.KAMMERER,D.KOSTREWA,J.ZURDO,A.DETKEN,C.GARCIA-ECHEVERRIA,         J.D.GREEN,S.A.MULLER,B.H.MEIER,F.K.WINKLER,C.M.DOBSON,M.O.STEINMETZ ;http://dx.doi.org/10.1073/pnas.0306786101;15070736.0
58;1SN9;relative of another de novo design;DE NOVO PROTEIN             ;10-MAR-04;M.H.ALI,E.PEISACH,K.N.ALLEN,B.IMPERIALI                              ;http://dx.doi.org/10.1073/pnas.0401245101;15302930.0
59;1SNA;relative of another de novo design;DE NOVO PROTEIN             ;10-MAR-04;M.H.ALI,E.PEISACH,K.N.ALLEN,B.IMPERIALI                              ;http://dx.doi.org/10.1073/pnas.0401245101;15302930.0
60;1SNE;relative of another de novo design;DE NOVO PROTEIN             ;10-MAR-04;M.H.ALI,E.PEISACH,K.N.ALLEN,B.IMPERIALI                              ;http://dx.doi.org/10.1073/pnas.0401245101;15302930.0
61;1T8J;relative of another de novo design;DE NOVO PROTEIN             ;13-MAY-04;M.D.STRUTHERS,J.J.OTTESEN,B.IMPERIALI                                ;http://dx.doi.org/10.1016/S1359-0278(98)00015-7;9565754.0
62;1TGG;original de novo design;DE NOVO PROTEIN             ;28-MAY-04;J.J.PLECS,P.B.HARBURY,P.S.KIM,T.ALBER                                ;http://dx.doi.org/10.1016/j.jmb.2004.06.051;15313624.0
63;1TJB;small, non-systematic, and other;DE NOVO PROTEIN             ;03-JUN-04;M.NITZ,M.SHERAWAT,K.J.FRANZ,E.PEISACH,K.N.ALLEN,B.IMPERIALI          ;http://dx.doi.org/10.1002/anie.200460028;15248272.0
64;1U0I;original de novo design;DE NOVO PROTEIN             ;13-JUL-04;D.A.LINDHOUT,J.R.LITOWSKI,P.MERCIER,R.S.HODGES,B.D.SYKES             ;http://dx.doi.org/10.1002/bip.20150;15457434.0
65;1U2U;relative of another de novo design;TRANSCRIPTION              ;20-JUL-04;D.N.MARTI,H.R.BOSSHARD                                               ;http://dx.doi.org/10.1021/bi048771t;15449933.0
66;1VL3;engineered;DE NOVO PROTEIN             ;05-JUL-04;A.LOMBARDI,F.NASTRI,D.MARASCO,O.MAGLIO,G.DE SANCTIS,F.SINIBALDI,      R.SANTUCCI,M.COLETTA,V.PAVONE                                       ;http://dx.doi.org/10.1002/chem.200304831;14639648.0
67;1VRZ;engineered;DE NOVO PROTEIN             ;14-OCT-05;RUDRESH,S.RAMAKUMAR,U.A.RAMAGOPAL,Y.INAI,D.SAHAL                     ;http://dx.doi.org/10.1016/j.str.2004.02.014;15016355.0
68;1XOF;engineered;DE NOVO PROTEIN             ;06-OCT-04;M.H.ALI,C.M.TAYLOR,G.GRIGORYAN,K.N.ALLEN,B.IMPERIALI,A.E.KEATING     ;http://dx.doi.org/10.1016/j.str.2004.12.009;15698566.0
69;1Y47;relative of another de novo design;DE NOVO PROTEIN             ;30-NOV-04;S.J.LAHR,D.E.ENGEL,S.E.STAYROOK,O.MAGLIO,B.NORTH,S.GEREMIA,           A.LOMBARDI,W.F.DEGRADO                                              ;http://dx.doi.org/10.1016/j.jmb.2004.12.016;15713492.0
70;1YMZ;original de novo design;UNKNOWN FUNCTION            ;22-JAN-05;M.SOCOLICH,S.W.LOCKLESS,W.P.RUSS,H.LEE,K.H.GARDNER,R.RANGANATHAN     ;http://dx.doi.org/10.1038/nature03991;16177782.0
71;2A3D;original de novo design;THREE-HELIX BUNDLE           ;01-APR-99;S.T.R.WALSH,H.CHENG,J.W.BRYSON,H.RODER,W.F.DEGRADO                   ;http://dx.doi.org/10.1073/pnas.96.10.5486;10318910.0
72;2AVP;relative of another de novo design;DE NOVO PROTEIN             ;30-AUG-05;T.KAJANDER,A.L.CORTAJARENA,E.R.MAIN,S.MOCHRIE,L.REGAN                ;http://dx.doi.org/10.1107/S0907444907024353;17582171.0
73;2BKG;relative of another de novo design;DE NOVO PROTEIN             ;16-FEB-05;H.K.BINZ,A.KOHL,A.PLUCKTHUN,M.G.GRUTTER                              ;http://dx.doi.org/10.1002/prot.20930;16493627.0
74;2BH8;evolved;TRANSCRIPTION              ;07-JAN-05;S.DE BONO,L.RIECHMANN,E.GIRARD,R.L.WILLIAMS,G.WINTER                 ;http://dx.doi.org/10.1073/pnas.0407298102;15671167.0
75;2CW1;original de novo design;DE NOVO PROTEIN             ;15-JUN-05;Y.ISOGAI,Y.ITO,T.IKEYA,Y.SHIRO,M.OTA                                 ;http://dx.doi.org/10.1016/j.jmb.2005.10.005;16289118.0
76;2EVQ;small, non-systematic, and other;DE NOVO PROTEIN             ;31-OCT-05;N.H.ANDERSEN,K.A.OLSEN,R.M.FESINMEYER                                ;http://dx.doi.org/10.1021/ja054971w;16669679.0
77;2FO7;relative of another de novo design;DE NOVO PROTEIN             ;12-JAN-06;T.KAJANDER,A.L.CORTAJARENA,L.REGAN                                   ;http://dx.doi.org/10.1107/S0907444907024353;17582171.0
78;2HYZ;relative of another de novo design;DE NOVO PROTEIN             ;08-AUG-06;T.KAJANDER,A.L.CORTAJARENA,L.REGAN                                   ;http://dx.doi.org/10.1107/S0907444907024353;17582171.0
79;2JAB;relative of another de novo design;DE NOVO PROTEIN             ;27-NOV-06;C.ZAHND,E.WYLER,J.M.SCHWENK,D.STEINER,M.C.LAWRENCE,N.M.MCKERN,        F.PECORARI,C.W.WARD,T.O.JOOS,A.PLUCKTHUN                            ;http://dx.doi.org/10.1016/j.jmb.2007.03.028;17466328.0
80;2JGO;original de novo design;DE NOVO PROTEIN             ;13-FEB-07;D.S.TOUW,C.E.NORDMAN,J.A.STUCKEY,V.L.PECORARO                        ;http://dx.doi.org/10.1073/pnas.0701979104;17609383.0
81;2JOF;relative of another de novo design;DE NOVO PROTEIN             ;09-MAR-07;B.BARUA,N.H.ANDERSEN                                                 ;http://dx.doi.org/10.1093/protein/gzm082;18203802.0
82;2JRE;relative of another de novo design;DE NOVO PROTEIN             ;25-JUN-07;C.LARSON,M.STIFFLER,P.LI,M.ROSEN,G.MACBEATH,R.RANGANATHAN            ;;
83;2JST;relative of another de novo design;DE NOVO PROTEIN             ;12-JUL-07;T.CUI,V.BONDARENKO,D.MA,C.CANLAS,N.R.BRANDON,J.S.JOHANSSON,P.TANG,    Y.XU                                                                ;http://dx.doi.org/10.1529/biophysj.107.117853;18310239.0
84;2JUA;original de novo design;DE NOVO PROTEIN             ;16-AUG-07;A.GO,S.KIM,J.S.BAUM,M.H.HECHT                                        ;http://dx.doi.org/10.1110/ps.073377908;18436954.0
85;2KDL;engineered;HUMAN SERUM ALBUMIN BINDING PROTEIN   ;12-JAN-09;Y.HE,P.ALEXANDER,Y.CHEN,P.BRYAN,J.ORBAN                              ;http://dx.doi.org/10.1073/pnas.0906408106;19923431.0
86;2KDM;engineered;IGG BINDING PROTEIN           ;12-JAN-09;Y.HE,P.ALEXANDER,Y.CHEN,P.BRYAN,J.ORBAN                              ;http://dx.doi.org/10.1073/pnas.0906408106;19923431.0
87;2KI0;original de novo design;DE NOVO PROTEIN             ;18-APR-09;H.LIANG,H.CHEN,K.FAN,P.WEI,X.GUO,C.JIN,C.ZENG,C.TANG,L.LAI           ;http://dx.doi.org/10.1002/anie.200805476;19347908.0
88;2KIK;relative of another de novo design;DE NOVO PROTEIN             ;06-MAY-09;O.MAGLIO,A.LOMBARDI                                                  ;;
89;2KL8;original de novo design;DE NOVO PROTEIN             ;30-JUN-09;G.LIU,N.KOGA,M.JIANG,R.KOGA,R.XIAO,C.CICCOSANTI,D.BAKER,              G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)      ;http://dx.doi.org/10.1038/nature11600;23135467.0
90;2LN3;original de novo design;DE NOVO PROTEIN             ;15-DEC-11;G.LIU,R.KOGA,N.KOGA,R.XIAO,H.LEE,H.JANJUA,E.KOHAN,T.B.ACTON,          J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;http://dx.doi.org/10.1038/nature11600;23135467.0
91;2LTA;original de novo design;DE NOVO PROTEIN             ;15-MAY-12;G.LIU,R.KOGA,N.KOGA,R.XIAO,K.PEDERSON,K.HAMILTON,E.KOHAN,T.B.ACTON,   G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;http://dx.doi.org/10.1038/nature11600;23135467.0
92;2LV8;original de novo design;DE NOVO PROTEIN             ;29-JUN-12;G.LIU,R.KOGA,N.KOGA,R.XIAO,K.PEDERSON,K.HAMILTON,C.CICCOSANTI,        T.B.ACTON,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL    GENOMICS CONSORTIUM (NESG)                                          ;http://dx.doi.org/10.1038/nature11600;23135467.0
93;2LVB;original de novo design;DE NOVO PROTEIN             ;30-JUN-12;G.LIU,N.KOGA,R.KOGA,R.XIAO,K.HAMILTON,E.KOHAN,T.B.ACTON,G.KORNHABER,  J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;http://dx.doi.org/10.1038/nature11600;23135467.0
94;2KPO;relative of another de novo design;DE NOVO PROTEIN             ;17-OCT-09;G.LIU,R.KOGA,N.KOGA,R.XIAO,K.HAMILTON,C.CICCOSANTI,T.B.ACTON,D.BAKER, G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)      ;;
95;2L69;relative of another de novo design;DE NOVO PROTEIN             ;17-NOV-10;G.LIU,N.KOGA,R.KOGA,R.XIAO,A.MAO,B.MAO,D.PATEL,C.CICCOSANTI,          K.HAMILTON,T.B.ACTON,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL     GENOMICS CONSORTIUM (NESG)                                          ;;
96;2L82;relative of another de novo design;DE NOVO PROTEIN             ;31-DEC-10;G.LIU,N.KOGA,R.KOGA,R.XIAO,K.HAMILTON,H.JANJUA,S.TONG,T.B.ACTON,      J.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS       CONSORTIUM (NESG)                                                   ;;
97;2LCI;relative of another de novo design;DE NOVO PROTEIN             ;29-APR-11;G.LIU,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,C.CICCOSANTI,H.LEE,T.B.ACTON,     J.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS       CONSORTIUM (NESG)                                                   ;;
98;2LND;relative of another de novo design;DE NOVO PROTEIN             ;23-DEC-11;G.LIU,N.KOGA,R.KOGA,R.XIAO,H.LEE,H.JANJUA,E.KOHAN,T.B.ACTON,          J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;;
99;2LR0;relative of another de novo design;DE NOVO PROTEIN             ;19-MAR-12;G.LIU,N.KOGA,R.KOGA,R.XIAO,H.LEE,H.JANJUA,E.KOHAN,T.B.ACTON,          J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;;
100;2LRH;relative of another de novo design;DE NOVO PROTEIN             ;30-MAR-12;G.LIU,N.KOGA,R.KOGA,R.XIAO,H.LEE,H.JANJUA,E.KOHAN,T.B.ACTON,          J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;;
101;2LSE;relative of another de novo design;DE NOVO PROTEIN             ;28-APR-12;B.SATHYAMOORTHY,S.PULAVARTI,G.MURPHY,J.L.MILLS,A.ELETSKI,B.S.DER,     M.C.MACHIUS,B.KUHLMAN,G.T.MONTELIONE,T.SZYPERSKI,NORTHEAST           STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
102;2MR5;relative of another de novo design;DE NOVO PROTEIN             ;01-JUL-14;S.PULAVARTI,L.NIVON,M.MAGLAQUI,H.JANJUA,L.MAO,R.XIAO,G.KORNHABER,     D.BAKER,G.MONTELIONE,T.SZYPERSKI,NORTHEAST STRUCTURAL GENOMICS       CONSORTIUM (NESG)                                                   ;;
103;2MR6;relative of another de novo design;DE NOVO PROTEIN             ;01-JUL-14;X.XU,L.NIVON,J.F.FEDERIZON,M.MAGLAQUI,H.JANJUA,L.MAO,R.XIAO,          G.KORNHABER,D.BAKER,G.T.MONTELIONE,T.SZYPERSKI,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;;
104;2MRA;relative of another de novo design;DE NOVO PROTEIN             ;02-JUL-14;S.V.S.R.K.PULAVARTI,Y.KIPNIS,D.SUKUMARAN,M.MAGLAQUI,H.JANJUA,L.MAO,   R.XIAO,G.KORNHABER,D.BAKER,G.T.MONTELIONE,T.SZYPERSKI,NORTHEAST      STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
105;2MTL;relative of another de novo design;DE NOVO PROTEIN             ;19-AUG-14;G.LIU,N.KOGA,R.KOGA,R.XIAO,K.HAMILTON,C.CICCOSANTI,S.SAHDEV,E.KOHAN,  T.B.ACTON,G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST   STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
106;2N2T;relative of another de novo design;UNKNOWN FUNCTION, STRUCTURAL GENOMICS  ;14-MAY-15;G.LIU,Y.LIN,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,K.HAMILTON,T.B.ACTON,       G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;;
107;2N2U;relative of another de novo design;UNKNOWN FUNCTION, STRUCTURAL GENOMICS  ;14-MAY-15;G.LIU,Y.LIN,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,K.HAMILTON,K.PEDERSON,      T.B.ACTON,G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST   STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
108;2N3Z;relative of another de novo design;STRUCTURAL GENOMICS, DE NOVO PROTEIN  ;15-JUN-15;G.LIU,Y.LIN,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,K.PEDERSON,T.B.ACTON,       G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;;
109;2N75;relative of another de novo design;DE NOVO PROTEIN             ;03-SEP-15;G.LIU,Y.LIN,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,K.PEDERSON,T.B.ACTON,       G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;;
110;2KLW;original de novo design;DE NOVO PROTEIN             ;09-JUL-09;J.A.FALLAS,V.GAUBA,J.D.HARTGERINK                                    ;http://dx.doi.org/10.1074/jbc.M109.014753;19625247.0
111;2MQ8;original de novo design;DE NOVO PROTEIN             ;12-JUN-14;G.LIU,Y.LIN,N.KOGA,R.KOGA,R.XIAO,H.JANJUA,K.PEDERSON,T.B.ACTON,       G.KORNHABER,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL  GENOMICS CONSORTIUM (NESG)                                          ;http://dx.doi.org/10.1073/pnas.1509508112;26396255.0
112;2MUZ;original de novo design;DE NOVO PROTEIN             ;18-SEP-14;T.WANG,N.JOH,Y.WU,W.F.DEGRADO,M.HONG                                 ;http://dx.doi.org/10.1126/science.1261172;25525248.0
113;2N1E;original de novo design;PROTEIN FIBRIL             ;30-MAR-15;K.NAGY-SMITH,E.MOORE,J.SCHNEIDER,R.TYCKO                             ;http://dx.doi.org/10.1073/pnas.1509313112;26216960.0
114;2N4N;small, non-systematic, and other;DE NOVO PROTEIN             ;24-JUN-15;V.M.KUNG,G.CORNILESCU,S.H.GELLMAN                                    ;http://dx.doi.org/10.1002/anie.201506448;26457984.0
115;2N6H;small, non-systematic, and other;DE NOVO PROTEIN             ;20-AUG-15;V.M.KUNG,G.CORNILESCU,S.H.GELLMAN                                    ;http://dx.doi.org/10.1002/anie.201506448;26457984.0
116;2N6I;small, non-systematic, and other;DE NOVO PROTEIN             ;20-AUG-15;V.M.KUNG,G.CORNILESCU,S.H.GELLMAN                                    ;http://dx.doi.org/10.1002/anie.201506448;26457984.0
117;2N8D;original de novo design;DE NOVO PROTEIN, ANTIMICROBIAL PEPTIDE ;13-OCT-15;M.PILLONG,M.BLATTER,G.SCHNEIDER                                      ;http://dx.doi.org/10.1002/smll.201701316;28799716.0
118;2N8I;original de novo design;DE NOVO PROTEIN             ;15-OCT-15;X.XU,A.ELETSKY,J.F.FEDERIZON,T.M.JACOBS,B.KUHLMAN,T.SZYPERSKI,        NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;http://dx.doi.org/10.1126/science.aad8036;27151863.0
119;2N8W;original de novo design;DE NOVO PROTEIN             ;27-OCT-15;A.ELETSKY,J.F.FEDERIZON,X.XU,S.PULAVARTI,T.M.JACOBS,B.KUHLMAN,        T.SZYPERSKI,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)         ;http://dx.doi.org/10.1126/science.aad8036;27151863.0
120;5E6G;original de novo design;DE NOVO PROTEIN             ;09-OCT-15;T.M.JACOBS,T.WILLIAMS,B.KUHLMAN                                      ;http://dx.doi.org/10.1126/science.aad8036;27151863.0
121;2NBL;small, non-systematic, and other;DE NOVO PROTEIN             ;05-MAR-16;V.M.KUNG                                                             ;;
122;2O6N;relative of another de novo design;DE NOVO PROTEIN             ;07-DEC-06;M.SALES,T.ALBER                                                      ;http://dx.doi.org/10.1110/ps.062702907;17766380.0
123;2P0X;evolved;DE NOVO PROTEIN             ;01-MAR-07;S.S.MANSY,J.W.SZOSTAK,J.C.CHAPUT                                     ;http://dx.doi.org/10.1016/j.jmb.2007.05.062;17583732.0
124;2QYJ;relative of another de novo design;DE NOVO PROTEIN             ;15-AUG-07;T.MERZ                                                               ;http://dx.doi.org/10.1016/j.jmb.2007.11.047;18155045.0
125;2RT4;engineered;DE NOVO PROTEIN             ;18-APR-13;H.WATANABE,K.YAMASAKI,S.HONDA                                        ;http://dx.doi.org/10.1074/jbc.M113.530592;24356963.0
126;2WQH;relative of another de novo design;DE NOVO PROTEIN             ;21-AUG-09;A.M.KRACHLER,A.SHARMA,C.KLEANTHOUS                                   ;http://dx.doi.org/10.1002/prot.22726;20455268.0
127;2X6P;relative of another de novo design;DE NOVO PROTEIN             ;18-FEB-10;S.CHAKRABORTY,D.S.TOUW,A.F.A.PEACOCK,J.A.STUCKEY,                     V.L.PECORARO                                                        ;http://dx.doi.org/10.1021/ja101812c;20825181.0
128;2XEH;relative of another de novo design;DE NOVO PROTEIN             ;14-MAY-10;M.KRAMER,S.K.WETZEL,A.PLUCKTHUN,P.MITTL,M.GRUTTER                    ;http://dx.doi.org/10.1016/j.jmb.2010.09.023;20851127.0
129;2XEN;relative of another de novo design;DE NOVO PROTEIN             ;17-MAY-10;M.KRAMER,S.K.WETZEL,A.PLUCKTHUN,P.MITTL,M.GRUTTER                    ;http://dx.doi.org/10.1016/j.jmb.2010.09.023;20851127.0
130;2XEE;relative of another de novo design;DE NOVO PROTEIN             ;14-MAY-10;M.KRAMER,S.K.WETZEL,A.PLUCKTHUN,P.MITTL,M.GRUTTER                    ;http://dx.doi.org/10.1016/j.jmb.2010.09.023;20851127.0
131;3AL1;relative of another de novo design;STRUCTURAL PROTEIN           ;26-OCT-98;W.R.PATTERSON,D.H.ANDERSON,W.F.DEGRADO,D.CASCIO,D.EISENBERG          ;http://dx.doi.org/10.1110/ps.8.7.1410;10422829.0
132;3CAY;original de novo design;DE NOVO PROTEIN             ;20-FEB-08;D.N.HO,N.C.POMROY,J.A.CUESTA-SEIJO,G.G.PRIVE                         ;http://dx.doi.org/10.1073/pnas.0801941105;18753631.0
133;3H5F;relative of another de novo design;DE NOVO PROTEIN             ;22-APR-09;A.F.A.PEACOCK,J.A.STUCKEY,V.L.PECORARO                               ;http://dx.doi.org/10.1002/anie.200902166;19579245.0
134;3H5G;relative of another de novo design;DE NOVO PROTEIN             ;22-APR-09;A.F.A.PEACOCK,J.A.STUCKEY,V.L.PECORARO                               ;http://dx.doi.org/10.1002/anie.200902166;19579245.0
135;3J89;original de novo design;SYNTHETIC PEPTIDE            ;07-OCT-14;E.H.EGELMAN,C.XU,F.DIMAIO,E.MAGNOTTI,C.MODLIN,X.YU,E.WRIGHT,D.BAKER,  V.P.CONTICELLO                                                      ;http://dx.doi.org/10.1016/j.str.2014.12.008;25620001.0
136;3LJM;relative of another de novo design;DE NOVO PROTEIN             ;26-JAN-10;S.CHAKRABORTY                                                        ;http://dx.doi.org/10.1021/ja101812c;20825181.0
137;3LTJ;original de novo design;PROTEIN BINDING             ;16-FEB-10;A.URVOAS,A.GUELLOUZ,M.GRAILLE,H.VAN TILBEURGH,M.DESMADRIL,P.MINARD   ;http://dx.doi.org/10.1016/j.jmb.2010.09.048;20887736.0
138;3LTM;original de novo design;PROTEIN BINDING             ;16-FEB-10;A.URVOAS,A.GUELLOUZ,M.GRAILLE,H.VAN TILBEURGH,M.DESMADRIL,P.MINARD   ;http://dx.doi.org/10.1016/j.jmb.2010.09.048;20887736.0
139;3NI3;small, non-systematic, and other;UNKNOWN FUNCTION            ;15-JUN-10;M.R.SAWAYA,D.EISENBERG,J.S.NOWICK,T.P.KORMAN,O.KHAKSHOOR             ;http://dx.doi.org/10.1021/ja103438w;20669960.0
140;3OVJ;small, non-systematic, and other;PROTEIN FIBRIL             ;16-SEP-10;M.LANDAU,D.EISENBERG                                                 ;http://dx.doi.org/10.1371/journal.pbio.1001080;21695112.0
141;3OW9;small, non-systematic, and other;PROTEIN FIBRIL             ;17-SEP-10;M.LANDAU,D.EISENBERG                                                 ;http://dx.doi.org/10.1073/pnas.1112600108;21949245.0
142;3P6I;relative of another de novo design;DE NOVO PROTEIN             ;11-OCT-10;M.BLABER,J.LEE                                                       ;;
143;3P6J;relative of another de novo design;DE NOVO PROTEIN             ;11-OCT-10;M.BLABER,J.LEE                                                       ;;
144;3Q7W;evolved;DE NOVO PROTEIN             ;05-JAN-11;M.BLABER,J.LEE                                                       ;http://dx.doi.org/10.1073/pnas.1219530110;23341608.0
145;3Q7X;relative of another de novo design;DE NOVO PROTEIN             ;05-JAN-11;M.BLABER,J.LEE                                                       ;http://dx.doi.org/10.1073/pnas.1219530110;23341608.0
146;3Q7Y;relative of another de novo design;DE NOVO PROTEIN             ;05-JAN-11;M.BLABER,J.LEE                                                       ;http://dx.doi.org/10.1073/pnas.1219530110;23341608.0
147;3R3K;original de novo design;DE NOVO PROTEIN             ;16-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
148;3R46;relative of another de novo design;DE NOVO PROTEIN             ;17-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
149;3R47;relative of another de novo design;DE NOVO PROTEIN             ;17-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
150;3R48;original de novo design;DE NOVO PROTEIN             ;17-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
151;3R4A;original de novo design;DE NOVO PROTEIN             ;17-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
152;3R4H;relative of another de novo design;DE NOVO PROTEIN             ;17-MAR-11;N.R.ZACCAI,B.H.C.CHI,D.N.WOOLFSON,R.L.BRADY                          ;http://dx.doi.org/10.1038/nchembio.692;22037471.0
153;3RA3;relative of another de novo design;DE NOVO PROTEIN             ;27-MAR-11;N.R.ZACCAI,T.H.SHARP,M.BRUNING,D.N.WOOLFSON,R.L.BRADY                ;http://dx.doi.org/10.1073/pnas.1118622109;22847414.0
154;3RFS;original de novo design;PROTEIN BINDING             ;06-APR-11;H.J.KIM,H.K.CHEONG,Y.H.JEON                                          ;http://dx.doi.org/10.1073/pnas.1113193109;22328160.0
155;3RFJ;original de novo design;PROTEIN BINDING             ;06-APR-11;H.J.KIM,H.K.CHEONG,Y.H.JEON                                          ;http://dx.doi.org/10.1073/pnas.1113193109;22328160.0
156;3S0R;original de novo design;DE NOVO PROTEIN             ;13-MAY-11;R.ACHARYA,G.GRIGORYAN,Y.H.KIM,W.F.DEGRADO                            ;http://dx.doi.org/10.1126/science.1198841;21617073.0
157;3T4F;relative of another de novo design;BIOSYNTHETIC PROTEIN          ;25-JUL-11;J.A.FALLAS,J.DONG,M.D.MILLER,Y.J.TAO,J.D.HARTGERINK                  ;http://dx.doi.org/10.1074/jbc.M111.296574;22179819.0
158;3TWE;relative of another de novo design;UNKNOWN FUNCTION            ;21-SEP-11;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1073/pnas.1120112109;22411812.0
159;3TWF;relative of another de novo design;UNKNOWN FUNCTION            ;21-SEP-11;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1073/pnas.1120112109;22411812.0
160;3TWG;relative of another de novo design;UNKNOWN FUNCTION            ;21-SEP-11;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1073/pnas.1120112109;22411812.0
161;3U1O;engineered;HYDROLASE                ;30-SEP-11;A.KUZIN,M.SU,S.LEW,F.FOROUHAR,J.SEETHARAMAN,P.DAYA,R.XIAO,            C.CICCOSANTI,F.RICHTER,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE, J.F.HUNT,L.TONG,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)     ;http://dx.doi.org/10.1021/ja3037367;22871159.0
162;3U1V;engineered;STRUCTURAL GENOMICS, UNKNOWN FUNCTION  ;30-SEP-11;A.KUZIN,M.SU,S.M.VOROBIEV,J.SEETHARAMAN,D.PATEL,R.XIAO,C.CICCOSANTI,  F.RICHTER,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,J.F.HUNT,     L.TONG,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)              ;http://dx.doi.org/10.1021/ja3037367;22871159.0
163;3U29;relative of another de novo design;BIOSYNTHETIC PROTEIN          ;02-OCT-11;J.A.FALLAS,J.DONG,M.D.MILLER,Y.J.TAO,J.D.HARTGERINK                  ;http://dx.doi.org/10.1074/jbc.M111.296574;22179819.0
164;3VJF;original de novo design;DE NOVO PROTEIN             ;18-OCT-11;R.ARAI,A.KIMURA,N.KOBAYASHI,K.MATSUO,T.SATO,A.F.WANG,J.M.PLATT,       L.H.BRADLEY,M.H.HECHT                                               ;http://dx.doi.org/10.1021/jp212438h;22397676.0
165;3V45;engineered;DE NOVO PROTEIN             ;14-DEC-11;A.KUZIN,M.SU,J.SEETHARAMAN,M.MAGLAQUI,R.XIAO,E.KOHAN,S.RAJAGOPALAN,   J.K.EVERETT,R.NAIR,T.B.ACTON,B.ROST,D.BAKER,G.T.MONTELIONE,L.TONG,   J.F.HUNT,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)            ;http://dx.doi.org/10.1038/nchembio.1498;24705591.0
166;3V86;original de novo design;DE NOVO PROTEIN             ;22-DEC-11;R.ACHARYA,B.NORTH,J.SAVEN,W.DEGRADO                                  ;http://dx.doi.org/10.1073/pnas.1112595109;22538812.0
167;3WN8;relative of another de novo design;STRUCTURAL PROTEIN           ;06-DEC-13;K.OKUYAMA,M.HAGA,K.NOGUCHI,T.TANAKA                                  ;http://dx.doi.org/10.1002/bip.22478;24615532.0
168;3WW7;original de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
169;3WW8;relative of another de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
170;3WW9;relative of another de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
171;3WWA;relative of another de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
172;3WWB;relative of another de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
173;3WWF;relative of another de novo design;DE NOVO PROTEIN             ;17-JUN-14;A.R.D.VOET,H.NOGUCHI,C.ADDY,D.SIMONCINI,D.TERADA,S.UNZAI,S.Y.PARK,    K.Y.J.ZHANG,J.R.H.TAME                                              ;http://dx.doi.org/10.1073/pnas.1412768111;25288768.0
174;4DAC;relative of another de novo design;DE NOVO PROTEIN             ;12-JAN-12;C.J.LANCI,C.M.MACDERMAID,J.G.SAVEN                                   ;http://dx.doi.org/10.1073/pnas.1112595109;22538812.0
175;4DBA;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-12;C.MADHURANTAKAM,G.VARADAMSETTY,M.G.GRUTTER,A.PLUCKTHUN,P.R.E.MITTL   ;http://dx.doi.org/10.1002/pro.2085;22544642.0
176;4DB6;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-12;C.MADHURANTAKAM,G.VARADAMSETTY,M.G.GRUTTER,A.PLUCKTHUN,P.R.E.MITTL   ;http://dx.doi.org/10.1002/pro.2085;22544642.0
177;4DB8;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-12;C.MADHURANTAKAM,G.VARADAMSETTY,M.G.GRUTTER,A.PLUCKTHUN,P.R.E.MITTL   ;http://dx.doi.org/10.1002/pro.2085;22544642.0
178;4DB9;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-12;C.MADHURANTAKAM,G.VARADAMSETTY,M.G.GRUTTER,A.PLUCKTHUN,P.R.E.MITTL   ;http://dx.doi.org/10.1002/pro.2085;22544642.0
179;4DZK;relative of another de novo design;DE NOVO PROTEIN             ;01-MAR-12;M.BRUNING,A.R.THOMSON,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON              ;http://dx.doi.org/10.1021/sb300028q;23651206.0
180;4DZL;original de novo design;DE NOVO PROTEIN             ;01-MAR-12;M.BRUNING,A.R.THOMSON,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON              ;http://dx.doi.org/10.1021/sb300028q;23651206.0
181;4DZM;original de novo design;DE NOVO PROTEIN             ;01-MAR-12;M.BRUNING,A.R.THOMSON,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON              ;http://dx.doi.org/10.1021/sb300028q;23651206.0
182;4DZN;relative of another de novo design;DE NOVO PROTEIN             ;01-MAR-12;M.BRUNING,A.R.THOMSON,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON              ;http://dx.doi.org/10.1021/sb300028q;23651206.0
183;4E0K;small, non-systematic, and other;PROTEIN FIBRIL             ;04-MAR-12;M.ZHAO,C.LIU,M.R.SAWAYA,D.EISENBERG                                  ;http://dx.doi.org/10.1073/pnas.1218792109;23213214.0
184;4E0L;small, non-systematic, and other;PROTEIN FIBRIL             ;04-MAR-12;M.ZHAO,C.LIU,S.R.MICHAEL,D.EISENBERG                                 ;http://dx.doi.org/10.1073/pnas.1218792109;23213214.0
185;4E0M;small, non-systematic, and other;PROTEIN FIBRIL             ;04-MAR-12;M.ZHAO,C.LIU,S.R.MICHAEL,D.EISENBERG                                 ;http://dx.doi.org/10.1073/pnas.1218792109;23213214.0
186;4E0N;small, non-systematic, and other;PROTEIN FIBRIL             ;04-MAR-12;M.ZHAO,C.LIU,S.R.MICHAEL,D.EISENBERG                                 ;http://dx.doi.org/10.1073/pnas.1218792109;23213214.0
187;4E0O;small, non-systematic, and other;PROTEIN FIBRIL             ;04-MAR-12;M.ZHAO,C.LIU,M.R.SAWAYA,D.EISENBERG                                  ;http://dx.doi.org/10.1073/pnas.1218792109;23213214.0
188;4E88;engineered;HYDROLASE                ;19-MAR-12;A.KUZIN,M.SU,J.SEETHARAMAN,X.XIAO,S.SAHDEV,C.CICCOSANTI,F.RICHTER,    J.K.EVERETT,T.B.ACTON,G.T.MONTELIONE,J.F.HUNT,L.TONG,NORTHEAST       STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
189;4ESS;engineered;DE NOVO PROTEIN, HYDROLASE       ;23-APR-12;A.KUZIN,M.SU,J.SEETHARAMAN,K.KORNHABER,G.KORNHABER,S.RAJAGOPALAN,     D.BAKER,J.K.EVERETT,T.B.ACTON,G.T.MONTELIONE,L.TONG,J.F.HUNT,        NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;http://dx.doi.org/10.1038/nchembio.1498;24705591.0
190;4G1A;original de novo design;METAL BINDING PROTEIN          ;10-JUL-12;S.NI,M.A.KENNEDY,M.Y.OGAWA                                           ;http://dx.doi.org/10.1016/j.jinorgbio.2012.10.010;23160144.0
191;4G3B;relative of another de novo design;DE NOVO PROTEIN             ;13-JUL-12;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1002/pro.2150;22930450.0
192;4G4L;relative of another de novo design;DE NOVO PROTEIN             ;16-JUL-12;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1002/pro.2150;22930450.0
193;4G4M;relative of another de novo design;DE NOVO PROTEIN             ;16-JUL-12;B.C.BUER,J.L.MEAGHER,J.A.STUCKEY,E.N.G.MARSH                         ;http://dx.doi.org/10.1002/pro.2150;22930450.0
194;4GVW;engineered;DE NOVO PROTEIN, HYDROLASE       ;31-AUG-12;A.KUZIN,S.LEW,J.SEETHARAMAN,S.RAJAGOPALAN,J.K.EVERETT,T.B.ACTON,      G.T.MONTELIONE,L.TONG,J.F.HUNT,NORTHEAST STRUCTURAL GENOMICS         CONSORTIUM (NESG)                                                   ;;
195;4H7R;relative of another de novo design;DE NOVO PROTEIN             ;20-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
196;4H8F;relative of another de novo design;DE NOVO PROTEIN             ;22-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
197;4H8G;relative of another de novo design;DE NOVO PROTEIN             ;22-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
198;4H8L;relative of another de novo design;DE NOVO PROTEIN             ;23-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
199;4H8M;relative of another de novo design;DE NOVO PROTEIN             ;23-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
200;4H8O;relative of another de novo design;DE NOVO PROTEIN             ;23-SEP-12;B.CHI,N.R.ZACCAI,R.L.BRADY,D.N.WOOLFSON                              ;;
201;4IVH;small, non-systematic, and other;DE NOVO PROTEIN             ;22-JAN-13;J.D.PHAM,N.CHIM,C.W.GOULDING,J.S.NOWICK                              ;;
202;4J4Z;engineered;STRUCTURAL GENOMICS, UNKNOWN FUNCTION  ;07-FEB-13;A.P.KUZIN,S.LEW,S.RAJAGOPALAN,M.MAGLAQUI,R.XIAO,D.LEE,J.K.EVERETT,    T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,J.F.HUNT,NORTHEAST           STRUCTURAL GENOMICS CONSORTIUM (NESG)                               ;;
203;4JBC;engineered;STRUCTURAL GENOMICS, UNKNOWN FUNCTION  ;19-FEB-13;A.KUZIN,S.LEW,S.RAJAGOPALAN,J.SEETHARAMAN,M.MAGLAQUI,R.XIAO,D.LEE,    J.K.EVERETT,T.B.ACTON,G.T.MONTELIONE,L.TONG,D.BAKER,J.F.HUNT,        NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;;
204;4JXI;engineered;STRUCTURAL GENOMICS, UNKNOWN FUNCTION  ;28-MAR-13;A.KUZIN,M.D.SMITH,F.RICHTER,S.LEW,R.SEETHARAMAN,C.BRYAN,Z.LECH,       G.KISS,R.MORETTI,M.MAGLAQUI,R.XIAO,E.KOHAN,M.SMITH,J.K.EVERETT,      R.NGUYEN,V.PANDE,D.HILVERT,G.KORNHABER,D.BAKER,G.T.MONTELIONE,       J.F.HUNT,L.TONG,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)     ;;
205;4JLR;original de novo design;IMMUNE SYSTEM              ;12-MAR-13;P.B.RUPERT,B.CORREIA,W.SCHIEF,R.K.STRONG                             ;http://dx.doi.org/10.1038/nature12966;24499818.0
206;4N9G;original de novo design;IMMUNE SYSTEM              ;21-OCT-13;C.T.D.CARRICO,R.K.STRONG                                             ;http://dx.doi.org/10.1038/nature12966;24499818.0
207;4L8I;original de novo design;IMMUNE SYSTEM              ;17-JUN-13;J.JARDINE,C.CORRENTI,M.A.HOLMES,R.K.STRONG,W.R.SCHIEF                ;http://dx.doi.org/10.1038/nature12966;24499818.0
208;4K0C;engineered;STRUCTURAL GENOMICS, UNKNOWN FUNCTION  ;03-APR-13;A.KUZIN,S.LEW,S.RAJAGOPALAN,J.SEETHARAMAN,M.MAGLAQUI,R.XIAO,D.LEE,    J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,J.F.HUNT,        NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;;
209;4O60;relative of another de novo design;DE NOVO PROTEIN             ;20-DEC-13;A.S.ETHAYATHULLA,L.GUAN                                              ;http://dx.doi.org/10.1038/srep08070;25627011.0
210;4P6J;relative of another de novo design;DE NOVO PROTEIN             ;25-MAR-14;N.H.JOH,R.ACHARYA,W.F.DEGRADO                                        ;http://dx.doi.org/10.1126/science.1261172;25525248.0
211;4P6K;relative of another de novo design;DE NOVO PROTEIN             ;25-MAR-14;N.H.JOH,R.ACHARYA,W.F.DEGRADO                                        ;http://dx.doi.org/10.1126/science.1261172;25525248.0
212;4P6L;relative of another de novo design;DE NOVO PROTEIN             ;25-MAR-14;N.H.JOH,R.ACHARYA,W.F.DEGRADO                                        ;http://dx.doi.org/10.1126/science.1261172;25525248.0
213;4PN8;original de novo design;DE NOVO PROTEIN             ;23-MAY-14;C.W.WOOD,A.J.BURTON,A.R.THOMSON,R.L.BRADY,D.N.WOOLFSON               ;http://dx.doi.org/10.1126/science.1257452;25342807.0
214;4PN9;original de novo design;DE NOVO PROTEIN             ;23-MAY-14;C.W.WOOD,A.J.BURTON,A.R.THOMSON,R.L.BRADY,D.N.WOOLFSON               ;http://dx.doi.org/10.1126/science.1257452;25342807.0
215;4PNA;original de novo design;DE NOVO PROTEIN             ;23-MAY-14;A.J.BURTON,C.W.WOOD,A.R.THOMSON,R.L.BRADY,D.N.WOOLFSON               ;http://dx.doi.org/10.1126/science.1257452;25342807.0
216;4PNB;relative of another de novo design;DE NOVO PROTEIN             ;23-MAY-14;C.W.WOOD,A.J.BURTON,A.R.THOMSON,R.L.BRADY,D.N.WOOLFSON               ;http://dx.doi.org/10.1126/science.1257452;25342807.0
217;4PND;relative of another de novo design;DE NOVO PROTEIN             ;23-MAY-14;C.W.WOOD,A.J.BURTON,A.R.THOMSON,R.L.BRADY,D.N.WOOLFSON               ;http://dx.doi.org/10.1126/science.1257452;25342807.0
218;4QFV;relative of another de novo design;DE NOVO PROTEIN             ;21-MAY-14;A.S.ETHAYATHULLA,E.B.TIKHONOVA,L.GUAN                                ;http://dx.doi.org/10.1038/srep08070;25627011.0
219;4QTR;original de novo design;DE NOVO DESIGN/DNA           ;08-JUL-14;Y.MOU,S.L.MAYO                                                       ;http://dx.doi.org/10.1038/nature14874;26331548.0
220;4NDL;original de novo design;DE NOVO PROTEIN             ;26-OCT-13;Y.MOU,P.S.HUANG,F.C.HSU,S.J.HUANG,S.L.MAYO                           ;http://dx.doi.org/10.1073/pnas.1505072112;26269568.0
221;4OYD;original de novo design;VIRAL PROTEIN/INHIBITOR         ;11-FEB-14;B.SHEN,E.PROCKO,D.BAKER,B.STODDARD                                   ;http://dx.doi.org/10.1016/j.cell.2014.04.034;24949974.0
222;4R80;original de novo design;DE NOVO PROTEIN             ;29-AUG-14;R.GUAN,E.MARCOS,P.O'CONNELL,J.SEETHARAMAN,H.JANJUA,R.XIAO,M.MAGLAQUI, T.B.ACTON,J.K.EVERETT,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL    GENOMICS CONSORTIUM (NESG)                                          ;;
223;4RJV;original de novo design;DE NOVO PROTEIN             ;09-OCT-14;P.T.O'CONNELL,Y.-R.LIN,R.GUAN,N.KOGA,R.KOGA,J.SEETHARAMAN,H.JANJUA,   R.XIAO,M.MAGLAQUI,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,      NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;;
224;5GAJ;original de novo design;DE NOVO PROTEIN             ;01-DEC-15;G.LIU,J.CASTELLLANOS,R.KOGA,N.KOGA,R.XIAO,K.PEDERSON,H.JANJUA,        E.KOHAN,T.B.ACTON,G.KORNHABER,J.EVERETT,D.BAKER,G.T.MONTELIONE,      NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;;
225;4U3H;original de novo design;DE NOVO PROTEIN             ;21-JUL-14;B.T.POREBSKI,S.MCGOWAN,A.M.BUCKLE                                    ;http://dx.doi.org/10.1093/protein/gzv002;25691761.0
226;4UOT;original de novo design;DE NOVO PROTEIN             ;09-JUN-14;G.OBERDORFER,P.HUANG,X.Y.PEI,C.XU,T.GONEN,B.NANNENGA,D.DIMAIO,        J.ROGERS,B.F.LUISI,D.BAKER                                          ;http://dx.doi.org/10.1126/science.1257481;25342806.0
227;4UOS;original de novo design;DE NOVO PROTEIN             ;09-JUN-14;G.OBERDORFER,P.HUANG,X.Y.PEI,C.XU,T.GONEN,B.NANNENGA,D.DIMAIO,        J.ROGERS,B.F.LUISI,D.BAKER                                          ;http://dx.doi.org/10.1126/science.1257481;25342806.0
228;4TQL;original de novo design;DE NOVO PROTEIN             ;11-JUN-14;B.L.NANNENGA,G.OBERDORFER,F.DIMAIO,D.BAKER,T.GONEN                   ;http://dx.doi.org/10.1126/science.1257481;25342806.0
229;4WSL;original de novo design;DE NOVO PROTEIN             ;28-OCT-14;S.C.COQUILLE,A.FILIPOVSKA,T.S.CHIA,L.RAJAPPA,J.P.LINGFORD,            M.F.M.RAZIF,S.THORE,O.RACKHAM                                       ;http://dx.doi.org/10.1038/ncomms6729;25517350.0
230;4PJQ;original de novo design;DE NOVO PROTEIN, RNA BINDING PROTEIN  ;12-MAY-14;S.C.COQUILLE,A.FILIPOVSKA,T.S.CHIA,L.RAJAPPA,J.P.LINGFORD,            M.F.M.RAZIF,S.THORE,O.RACKHAM                                       ;http://dx.doi.org/10.1038/ncomms6729;25517350.0
231;4PJR;original de novo design;DE NOVO PROTEIN, RNA BINDING PROTEIN  ;12-MAY-14;S.C.COQUILLE,A.FILIPOVSKA,T.S.CHIA,L.RAJAPPA,J.P.LINGFORD,            M.F.M.RAZIF,S.THORE,O.RACKHAM                                       ;http://dx.doi.org/10.1038/ncomms6729;25517350.0
232;4PJS;original de novo design;DE NOVO PROTEIN, RNA BINDING PROTEIN  ;12-MAY-14;S.C.COQUILLE,A.FILIPOVSKA,T.S.CHIA,L.RAJAPPA,J.P.LINGFORD,            M.F.M.RAZIF,S.THORE,O.RACKHAM                                       ;http://dx.doi.org/10.1038/ncomms6729;25517350.0
233;4WN4;original de novo design;DE NOVO PROTEIN             ;10-OCT-14;S.C.COQUILLE,A.FILIPOVSKA,T.S.CHIA,L.RAJAPPA,J.P.LINGFORD,            M.F.M.RAZIF,S.THORE,O.RACKHAM                                       ;http://dx.doi.org/10.1038/ncomms6729;25517350.0
234;4YXX;original de novo design;DE NOVO PROTEIN             ;23-MAR-15;L.DOYLE,J.BOLDUC,B.L.STODDARD,P.BRADLEY                              ;http://dx.doi.org/10.1038/nature16191;26675735.0
235;4YXY;original de novo design;DE NOVO PROTEIN             ;23-MAR-15;L.DOYLE,B.L.STODDARD,P.BRADLEY                                       ;http://dx.doi.org/10.1038/nature16191;26675735.0
236;4YXZ;relative of another de novo design;DE NOVO PROTEIN             ;23-MAR-15;L.DOYLE,B.L.STODDARD,P.BRADLEY                                       ;http://dx.doi.org/10.1038/nature16191;26675735.0
237;4YY2;original de novo design;DE NOVO PROTEIN             ;23-MAR-15;J.P.HALLINAN,P.BRADLEY,B.L.STODDARD                                  ;http://dx.doi.org/10.1038/nature16191;26675735.0
238;4YY5;relative of another de novo design;DE NOVO PROTEIN             ;23-MAR-15;J.P.HALLINAN,P.BRADLEY,B.L.STODDARD                                  ;http://dx.doi.org/10.1038/nature16191;26675735.0
239;4Z1R;small, non-systematic, and other;STRUCTURAL PROTEIN           ;27-MAR-15;M.E.PLONSKA-BRZEZINSKA,J.CZYRKO,D.M.BRUS,M.IMIERSKA,K.BRZEZINSKI     ;http://dx.doi.org/10.1039/c5ra15469c;
240;4ZCN;relative of another de novo design;DE NOVO PROTEIN             ;16-APR-15;A.R.D.VOET,H.NOGUCHI,C.ADDY,K.Y.J.ZHANG,J.R.H.TAME                   ;;
241;5BVB;engineered;DE NOVO PROTEIN             ;04-JUN-15;L.A.DOYLE,B.L.STODDARD                                               ;http://dx.doi.org/10.1021/acs.jcim.5b00387;26419257.0
242;5BVL;original de novo design;DE NOVO PROTEIN             ;05-JUN-15;K.FELDMEIER,B.HOCKER                                                 ;http://dx.doi.org/10.1038/nchembio.1966;26595462.0
243;6WVS;original de novo design;DE NOVO PROTEIN             ;06-MAY-20;M.J.BICK,I.C.HAYDON,S.J.CALDWELL,C.ZEYMER,P.HUANG,D.A.FERNANDEZ-      VELASCO,D.BAKER                                                     ;http://dx.doi.org/10.1073/pnas.2008535117;33203677.0
244;5BYO;original de novo design;DE NOVO PROTEIN             ;10-JUN-15;L.DOYLE,B.L.STODDARD,P.BRADLEY                                       ;http://dx.doi.org/10.1038/nature16191;26675735.0
245;5CHB;original de novo design;DE NOVO PROTEIN             ;10-JUL-15;A.R.D.VOET,H.NOGUCHI,C.ADDY,K.Y.J.ZHANG,J.R.H.TAME                   ;http://dx.doi.org/10.1002/anie.201503575;26136355.0
246;5CWB;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
247;5CWD;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
248;5CWC;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
249;5CWF;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
250;5CWH;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
251;5CWG;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
252;5CWQ;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
253;5CWJ;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
254;5CWI;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
255;5CWL;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
256;5CWK;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
257;5CWM;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
258;5CWN;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
259;5CWO;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
260;5CWP;original de novo design;DE NOVO PROTEIN             ;28-JUL-15;G.BHABHA,D.C.EKIERT                                                  ;http://dx.doi.org/10.1038/nature16162;26675729.0
261;5EHB;original de novo design;DE NOVO PROTEIN             ;28-OCT-15;R.LIZATOVIC,O.AURELIUS,O.STENSTROM,T.DRAKENBERG,M.AKKE,D.T.LOGAN,     I.ANDRE                                                             ;http://dx.doi.org/10.1016/j.str.2016.03.027;27161978.0
262;5EON;original de novo design;DE NOVO PROTEIN             ;10-NOV-15;R.K.SPENCER,A.I.HOCHBAUM                                             ;http://dx.doi.org/10.1021/acs.biochem.6b00201;27192036.0
263;5ET3;original de novo design;DE NOVO PROTEIN             ;17-NOV-15;K.-H.KIM,Y.H.KIM,R.ACHARYA,N.H.KIM,J.PAUL,G.GRIGORYAN,W.F.DEGRADO    ;http://dx.doi.org/10.1038/ncomms11429;27113637.0
264;5EZ8;relative of another de novo design;DE NOVO PROTEIN             ;26-NOV-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
265;5EZ9;relative of another de novo design;DE NOVO PROTEIN             ;26-NOV-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
266;5EZA;relative of another de novo design;DE NOVO PROTEIN             ;26-NOV-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
267;5EZC;original de novo design;DE NOVO PROTEIN             ;26-NOV-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
268;5EZE;relative of another de novo design;DE NOVO PROTEIN             ;26-NOV-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
269;5F2Y;relative of another de novo design;DE NOVO PROTEIN             ;02-DEC-15;A.J.BURTON,R.L.BRADY,D.N.WOOLFSON                                    ;http://dx.doi.org/10.1038/nchem.2555;27554410.0
270;5F53;relative of another de novo design;DE NOVO PROTEIN             ;04-DEC-15;A.R.D.VOET,J.R.H.TAME                                                ;;
271;5HKN;relative of another de novo design;DE NOVO PROTEIN             ;14-JAN-16;J.PAUL,R.ACHARYA,K.-H.KIM,Y.H.KIM,N.H.KIM,G.GRIGORYAN,W.F.DEGARDO    ;http://dx.doi.org/10.1038/ncomms11429;27113637.0
272;5HKR;relative of another de novo design;DE NOVO PROTEIN             ;14-JAN-16;R.ACHARYA,Y.H.KIM,G.GRIGORYAN,W.F.DEGARDO                            ;http://dx.doi.org/10.1038/ncomms11429;27113637.0
273;5J2L;original de novo design;DE NOVO PROTEIN             ;29-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
274;5J73;original de novo design;DE NOVO PROTEIN             ;05-APR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
275;5IZS;original de novo design;DE NOVO PROTEIN             ;25-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,G.OBERDORFER;http://dx.doi.org/10.1126/science.aad8865;27151862.0
276;5J0L;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
277;5J0K;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
278;5J10;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
279;5J0H;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
280;5J0J;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
281;5J0I;original de novo design;DE NOVO PROTEIN             ;28-MAR-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,S.BOYKEN,Z.CHEN,B.GROVES,    R.A.LANGAN,G.OBERDORFER,A.FORD,J.GILMORE,C.XU,F.DIMAIO,G.SEELIG     ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
282;5JQZ;original de novo design;DE NOVO PROTEIN             ;05-MAY-16;B.SANKARAN,P.H.ZWART,J.H.PEREIRA,D.BAKER,G.OBERDORFER,S.E.BOYKEN,     Z.CHEN                                                              ;http://dx.doi.org/10.1126/science.aad8865;27151862.0
283;5K7V;original de novo design;DE NOVO PROTEIN             ;26-MAY-16;B.SANKARAN,P.H.ZWART,J.A.FALLAS,J.H.PEREIRA,G.UEDA,D.BAKER           ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
284;4HB5;original de novo design;DE NOVO PROTEIN             ;27-SEP-12;S.VOROBIEV,M.SU,F.PARMEGGIANI,J.SEETHARAMAN,P.-S.HUANG,M.MAGLAQUI,    R.XIAO,D.LEE,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,    J.F.HUNT,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)            ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
285;4GPM;original de novo design;DE NOVO PROTEIN             ;21-AUG-12;S.VOROBIEV,M.SU,F.PARMEGGIANI,J.SEETHARAMAN,P.-S.HUANG,M.MAGLAQUI,    R.XIAO,D.LEE,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,    J.F.HUNT,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)            ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
286;4HXT;original de novo design;DE NOVO PROTEIN             ;12-NOV-12;S.VOROBIEV,M.SU,F.PARMEGGIANI,J.SEETHARAMAN,P.-S.HUANG,M.MAGLAQUI,    X.XIAO,D.LEE,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,    J.F.HUNT,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)            ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
287;4GMR;original de novo design;DE NOVO PROTEIN             ;16-AUG-12;S.VOROBIEV,M.SU,F.PARMEGGIANI,J.SEETHARAMAN,P.-S.HUANG,M.MAO,R.XIAO,  D.LEE,J.K.EVERETT,T.B.ACTON,D.BAKER,G.T.MONTELIONE,L.TONG,J.F.HUNT,  NORTHEAST STRUCTURAL GENOMICS CONSORTIUM (NESG)                     ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
288;5KWD;original de novo design;DE NOVO PROTEIN             ;17-JUL-16;J.A.FALLAS,B.SANKARAN,P.ZWART,D.BAKER                                ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
289;5HRZ;original de novo design;DE NOVO PROTEIN             ;24-JAN-16;D.CASCIO,D.E.MCNAMARA,J.A.FALLAS,D.BAKER,T.O.YEATES                  ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
290;5HRY;original de novo design;DE NOVO PROTEIN             ;24-JAN-16;D.CASCIO,D.E.MCNAMARA,J.A.FALLAS,D.BAKER,T.O.YEATES                  ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
291;5HS0;original de novo design;DE NOVO PROTEIN             ;24-JAN-16;D.E.MCNAMARA,D.CASCIO,J.A.FALLAS,D.BAKER,T.O.YEATES                  ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
292;5KBA;original de novo design;DE NOVO PROTEIN             ;02-JUN-16;B.SANKARAN,P.H.ZWART,J.A.FALLAS,J.H.PEREIRA,G.UEDA,D.BAKER           ;http://dx.doi.org/10.1038/nchem.2673;28338692.0
293;5K92;relative of another de novo design;DE NOVO PROTEIN             ;31-MAY-16;L.RUCKTHONG,M.L.ZASTROW,J.A.STUCKEY,V.L.PECORARO                     ;http://dx.doi.org/10.1021/jacs.6b07165;27532255.0
294;5KB0;relative of another de novo design;DE NOVO PROTEIN             ;02-JUN-16;L.RUCKTHONG,M.L.ZASTROW,J.A.STUCKEY,V.L.PECORARO                     ;http://dx.doi.org/10.1021/jacs.6b07165;27532255.0
295;5KB1;relative of another de novo design;DE NOVO PROTEIN             ;02-JUN-16;L.RUCKCTHONG,M.L.ZASTROW,J.A.STUCKEY,V.L.PECORARO                    ;http://dx.doi.org/10.1021/jacs.6b07165;27532255.0
296;5KB2;relative of another de novo design;DE NOVO PROTEIN             ;02-JUN-16;L.RUCKTHONG,M.L.ZASTROW,J.A.STUCKEY,V.L.PECORARO                     ;http://dx.doi.org/10.1021/jacs.6b07165;27532255.0
297;5KPE;original de novo design;DE NOVO PROTEIN             ;03-JUL-16;Y.TANG,G.LIU,D.BAKER,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS     CONSORTIUM (NESG)                                                   ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
298;5KPH;original de novo design;DE NOVO PROTEIN             ;04-JUL-16;Y.TANG,G.LIU,G.T.MONTELIONE,NORTHEAST STRUCTURAL GENOMICS CONSORTIUM  (NESG)                                                              ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
299;5L33;original de novo design;DE NOVO PROTEIN             ;03-AUG-16;G.OBERDORFER,E.MARCOS,B.BASANTA,T.M.CHIDYAUSIKU,B.SANKARAN,D.BAKER   ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
300;5TPJ;original de novo design;DE NOVO PROTEIN             ;20-OCT-16;B.BASANTA,G.OBERDORFER,E.MARCOS,T.M.CHIDYAUSIKU,B.SANKARAN,D.BAKER   ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
301;5TPH;original de novo design;DE NOVO PROTEIN             ;20-OCT-16;B.BASANTA,E.MARCOS,G.OBERDORFER,T.M.CHIDYAUSIKU,B.SANKARAN,D.BAKER   ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
302;5U35;original de novo design;DE NOVO PROTEIN             ;01-DEC-16;G.OBERDORFER,E.MARCOS,B.BASANTA,T.M.CHIDYAUSIKU,B.SANKARAN,P.H.ZWART, D.BAKER                                                             ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
303;5TS4;original de novo design;DE NOVO PROTEIN             ;27-OCT-16;B.BASANTA,G.OBERDORFER,T.M.CHIDYAUSIKU,E.MARCOS,J.H.PEREIRA,          B.SANKARAN,P.H.ZWART,D.BAKER                                        ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
304;5TRV;original de novo design;DE NOVO PROTEIN             ;27-OCT-16;B.BASANTA,G.OBERDORFER,E.MARCOS,T.M.CHIDYAUSIKU,B.SANKARAN,D.BAKER   ;http://dx.doi.org/10.1126/science.aah7389;28082595.0
305;5KVN;original de novo design;DE NOVO PROTEIN             ;14-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
306;2ND3;original de novo design;DE NOVO PROTEIN             ;22-APR-16;S.V.PULAVARTI,C.D.BAHL,J.M.GILMORE,A.ELETSKY,G.W.BUCHKO,D.BAKER,      T.SZYPERSKI                                                         ;http://dx.doi.org/10.1038/nature19791;27626386.0
307;2ND2;original de novo design;DE NOVO PROTEIN             ;22-APR-16;S.V.PULAVARTI,A.ELETSKY,C.D.BAHL,G.W.BUCHKO,D.BAKER,T.SZYPERSKI      ;http://dx.doi.org/10.1038/nature19791;27626386.0
308;5KWO;relative of another de novo design;DE NOVO PROTEIN             ;18-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
309;5KWP;relative of another de novo design;DE NOVO PROTEIN             ;18-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
310;5KWX;relative of another de novo design;DE NOVO PROTEIN             ;19-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
311;5KWZ;relative of another de novo design;DE NOVO PROTEIN             ;19-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
312;5KX0;relative of another de novo design;DE NOVO PROTEIN             ;19-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
313;5KX1;relative of another de novo design;DE NOVO PROTEIN             ;19-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
314;5KX2;relative of another de novo design;DE NOVO PROTEIN             ;19-JUL-16;P.J.HARVEY,D.J.CRAIK                                                 ;http://dx.doi.org/10.1038/nature19791;27626386.0
315;5LO3;original de novo design;STRUCTURAL PROTEIN           ;08-AUG-16;E.G.BAKER,K.L.HUDSON,C.WILLIAMS,G.G.BARTLETT,J.W.HEAL,R.B.SESSIONS,   M.P.CRUMP,D.N.WOOLFSON                                              ;http://dx.doi.org/10.1038/nchembio.2380;28530710.0
316;5LO2;original de novo design;STRUCTURAL PROTEIN           ;08-AUG-16;E.G.BAKER,K.L.HUDSON,C.WILLIAMS,G.G.BARTLETT,J.W.HEAL,R.B.SESSIONS,   M.P.CRUMP,D.N.WOOLFSON                                              ;http://dx.doi.org/10.1038/nchembio.2380;28530710.0
317;5LO4;original de novo design;STRUCTURAL PROTEIN           ;08-AUG-16;E.G.BAKER,C.WILLIAMS,K.L.HUDSON,G.G.BARTLETT,J.W.HEAL,R.B.SESSIONS,   M.P.CRUMP,D.N.WOOLFSON                                              ;http://dx.doi.org/10.1038/nchembio.2380;28530710.0
318;5TGY;original de novo design;UNKNOWN FUNCTION            ;28-SEP-16;N.F.POLIZZI,Y.WU                                                     ;http://dx.doi.org/10.1038/nchem.2846;29168496.0
319;5TGW;original de novo design;UNKNOWN FUNCTION            ;28-SEP-16;N.F.POLIZZI,Y.WU                                                     ;http://dx.doi.org/10.1038/nchem.2846;29168496.0
320;5U59;original de novo design;DE NOVO PROTEIN             ;06-DEC-16;N.A.TAVENOR,M.J.MURNIN,W.S.HORNE                                     ;http://dx.doi.org/10.1021/jacs.7b00651;28161945.0
321;5U5A;relative of another de novo design;DE NOVO PROTEIN             ;06-DEC-16;N.A.TAVENOR,M.J.MURNIN,W.S.HORNE                                     ;http://dx.doi.org/10.1021/jacs.7b00651;28161945.0
322;5U5B;original de novo design;DE NOVO PROTEIN             ;06-DEC-16;N.A.TAVENOR,M.J.MURNIN,W.S.HORNE                                     ;http://dx.doi.org/10.1021/jacs.7b00651;28161945.0
323;5U5C;original de novo design;DE NOVO PROTEIN             ;06-DEC-16;N.A.TAVENOR,M.J.MURNIN,W.S.HORNE                                     ;http://dx.doi.org/10.1021/jacs.7b00651;28161945.0
324;5U9T;relative of another de novo design;DE NOVO PROTEIN             ;18-DEC-16;L.RUCKTHONG,A.F.A.PEACOCK,J.A.STUCKEY,V.L.PECORARO                   ;http://dx.doi.org/10.1002/chem.201700660;28384393.0
325;5U9U;relative of another de novo design;DE NOVO PROTEIN             ;18-DEC-16;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;http://dx.doi.org/10.1002/chem.201700660;28384393.0
326;5UGK;small, non-systematic, and other;METAL BINDING PROTEIN          ;09-JAN-17;M.LEE,T.WANG,O.V.MAKHLYNETS,Y.WU,N.POLIZZI,H.WU,P.M.GOSAVI,           I.V.KORENDOVYCH,W.F.DEGRADO,M.HONG                                  ;http://dx.doi.org/10.1073/pnas.1706179114;28566494.0
327;5UXT;relative of another de novo design;DE NOVO PROTEIN             ;23-FEB-17;M.S.SMITH,W.M.BILLINGS,F.G.WHITBY,M.B.MILLER,J.L.PRICE               ;http://dx.doi.org/10.1039/c7ob01198a;28678274.0
328;5V2G;original de novo design;DE NOVO PROTEIN             ;03-MAR-17;B.DANG,H.WU,V.K.MULLIGAN,M.MRAVIC,Y.WU,T.LEMMIN,A.FORD,D.SILVA,       D.BAKER,W.F.DEGRADO                                                 ;http://dx.doi.org/10.1073/pnas.1710695114;28973862.0
329;5V2O;relative of another de novo design;DE NOVO PROTEIN             ;06-MAR-17;B.DANG,H.WU,V.K.MULLIGAN,M.MRAVIC,Y.WU,T.LEMMIN,A.FORD,D.SILVA,       D.BAKER,W.F.DEGRADO                                                 ;http://dx.doi.org/10.1073/pnas.1710695114;28973862.0
330;5VSG;small, non-systematic, and other;DE NOVO PROTEIN             ;11-MAY-17;S.MONDAL,M.R.SAWAYA,D.S.EISENBERG,E.GAZIT                            ;http://dx.doi.org/10.1021/jacs.8b10289;30532955.0
331;5UYO;original de novo design;DE NOVO PROTEIN             ;24-FEB-17;A.LEMAK,G.J.ROCKLIN,S.HOULISTON,L.CARTER,T.M.CHIDYAUSIKU,D.BAKER,     C.H.ARROWSMITH                                                      ;http://dx.doi.org/10.1126/science.aan0693;28706065.0
332;5UOI;original de novo design;DE NOVO PROTEIN             ;31-JAN-17;S.HOULISTON,G.J.ROCKLIN,A.LEMAK,L.CARTER,T.M.CHIDYAUSIKU,D.BAKER,     C.H.ARROWSMITH                                                      ;http://dx.doi.org/10.1126/science.aan0693;28706065.0
333;5UP1;original de novo design;DE NOVO PROTEIN             ;01-FEB-17;S.HOULISTON,G.J.ROCKLIN,A.LEMAK,L.CARTER,T.M.CHIDYAUSIKU,D.BAKER,     C.H.ARROWSMITH                                                      ;http://dx.doi.org/10.1126/science.aan0693;28706065.0
334;5UP5;original de novo design;DE NOVO PROTEIN             ;01-FEB-17;S.HOULISTON,G.J.ROCKLIN,A.LEMAK,L.CARTER,T.M.CHIDYAUSIKU,D.BAKER,     C.H.ARROWSMITH                                                      ;http://dx.doi.org/10.1126/science.aan0693;28706065.0
335;5VID;original de novo design;TOXIN                  ;15-APR-17;R.JIN,K.LAM,G.YAO                                                    ;http://dx.doi.org/10.1038/nature23912;28953867.0
336;5VLI;original de novo design;VIRAL PROTEIN/DE NOVO PROTEIN      ;25-APR-17;S.M.BERNARD,I.A.WILSON                                               ;http://dx.doi.org/10.1038/nature23912;28953867.0
337;5VMR;original de novo design;TOXIN                  ;28-APR-17;R.JIN,K.LAM,G.YAO                                                    ;http://dx.doi.org/10.1038/nature23912;28953867.0
338;5VTE;original de novo design;DE NOVO PROTEIN             ;16-MAY-17;R.K.SPENCER,A.I.HOCHBAUM                                             ;http://dx.doi.org/10.1021/acs.biochem.7b00756;28876052.0
339;5W0J;relative of another de novo design;DE NOVO PROTEIN             ;30-MAY-17;R.K.SPENCER,A.I.HOCHBAUM                                             ;http://dx.doi.org/10.1021/acs.biochem.7b00756;28876052.0
340;5WLJ;relative of another de novo design;DE NOVO PROTEIN             ;27-JUL-17;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1021/jacs.7b08261;29249157.0
341;5WLK;relative of another de novo design;DE NOVO PROTEIN             ;27-JUL-17;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1021/jacs.7b08261;29249157.0
342;5WLL;relative of another de novo design;DE NOVO PROTEIN             ;27-JUL-17;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1021/jacs.7b08261;29249157.0
343;5WLM;relative of another de novo design;DE NOVO PROTEIN             ;27-JUL-17;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1021/jacs.7b08261;29249157.0
344;5WOC;relative of another de novo design;DE NOVO PROTEIN             ;01-AUG-17;H.WU,Y.WU,W.F.DEGRADO                                                ;http://dx.doi.org/10.1073/pnas.1710695114;28973862.0
345;5WOD;relative of another de novo design;DE NOVO PROTEIN             ;01-AUG-17;H.WU,Y.WU,W.F.DEGRADO                                                ;http://dx.doi.org/10.1073/pnas.1710695114;28973862.0
346;5YAN;relative of another de novo design;STRUCTURAL PROTEIN           ;01-SEP-17;S.FAN                                                                ;http://dx.doi.org/10.1073/pnas.1802171115;29844169.0
347;6ANF;small, non-systematic, and other;DE NOVO PROTEIN             ;13-AUG-17;H.WU,A.ACHARYYA,Y.WU,L.LIU,H.JO,F.GAI,W.F.DEGRADO                    ;http://dx.doi.org/10.1002/cbic.201800026;29417711.0
348;6B17;small, non-systematic, and other;DE NOVO PROTEIN             ;17-SEP-17;H.WU,A.ACHARYYA,Y.WU,L.LIU,H.JO,F.GAI,W.F.DEGRADO                    ;http://dx.doi.org/10.1002/cbic.201800026;29417711.0
349;6B85;original de novo design;MEMBRANE PROTEIN            ;05-OCT-17;P.LU,F.DIMAIO,D.MIN,J.BOWIE,K.Y.WEI,D.BAKER                          ;http://dx.doi.org/10.1126/science.aaq1739;29496880.0
350;6B87;original de novo design;MEMBRANE PROTEIN            ;05-OCT-17;P.LU,F.DIMAIO,D.MIN,K.Y.WEI,J.BOWIE,D.BAKER                          ;http://dx.doi.org/10.1126/science.aaq1739;29496880.0
351;6C4X;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-18;L.LIU,S.Q.ZHANG                                                      ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
352;6C4Y;original de novo design;DE NOVO PROTEIN             ;13-JAN-18;L.LIU,S.Q.ZHANG                                                      ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
353;6C4Z;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-18;L.LIU,S.Q.ZHANG                                                      ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
354;6C51;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-18;L.LIU,S.Q.ZHANG                                                      ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
355;6C52;relative of another de novo design;DE NOVO PROTEIN             ;13-JAN-18;L.LIU,S.Q.ZHANG                                                      ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
356;6CFA;small, non-systematic, and other;DE NOVO PROTEIN             ;14-FEB-18;E.S.F.ALVES,L.M.LIAO                                                 ;;
357;6D02;relative of another de novo design;DE NOVO PROTEIN             ;10-APR-18;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
358;6D0T;original de novo design;DE NOVO PROTEIN             ;10-APR-18;J.DOU,A.A.VOROBIEVA,W.SHEFFLER,L.A.DOYLE,H.PARK,M.J.BICK,B.MAO,       G.W.FOIGHT,M.LEE,L.CARTER,B.SANKARAN,S.OVCHINNIKOV,E.MARCOS,P.HUANG, J.C.VAUGHAN,B.L.STODDARD,D.BAKER                                    ;http://dx.doi.org/10.1038/s41586-018-0509-0;30209393.0
359;6CZH;original de novo design;DE NOVO PROTEIN             ;09-APR-18;L.A.DOYLE,B.L.STODDARD                                               ;http://dx.doi.org/10.1038/s41586-018-0509-0;30209393.0
360;6CZG;original de novo design;DE NOVO PROTEIN             ;09-APR-18;L.A.DOYLE,B.L.STODDARD                                               ;http://dx.doi.org/10.1038/s41586-018-0509-0;30209393.0
361;6CZJ;original de novo design;DE NOVO PROTEIN             ;09-APR-18;L.A.DOYLE,B.L.STODDARD                                               ;http://dx.doi.org/10.1038/s41586-018-0509-0;30209393.0
362;6CZI;original de novo design;DE NOVO PROTEIN             ;09-APR-18;L.A.DOYLE,B.L.STODDARD                                               ;http://dx.doi.org/10.1038/s41586-018-0509-0;30209393.0
363;6D37;engineered;DE NOVO PROTEIN             ;14-APR-18;A.BYRNE,N.H.ANDERSEN                                                 ;http://dx.doi.org/10.1002/bip.23260;30779444.0
364;6E5H;relative of another de novo design;DE NOVO PROTEIN             ;20-JUL-18;C.C.CABALTEJA,D.S.MIHALKO,W.S.HORNE                                  ;http://dx.doi.org/10.1002/cbic.201800558;30326175.0
365;6E5I;relative of another de novo design;DE NOVO PROTEIN             ;20-JUL-18;C.C.CABALTEJA,D.S.MIHALKO,W.S.HORNE                                  ;http://dx.doi.org/10.1002/cbic.201800558;30326175.0
366;6E5J;relative of another de novo design;DE NOVO PROTEIN             ;20-JUL-18;C.C.CABALTEJA,D.S.MIHALKO,W.S.HORNE                                  ;http://dx.doi.org/10.1002/cbic.201800558;30326175.0
367;6E5K;relative of another de novo design;DE NOVO PROTEIN             ;20-JUL-18;C.C.CABALTEJA,D.S.MIHALKO,W.S.HORNE                                  ;http://dx.doi.org/10.1002/cbic.201800558;30326175.0
368;6EGL;relative of another de novo design;DE NOVO PROTEIN             ;20-AUG-18;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;http://dx.doi.org/10.1002/chem.201806040;30861211.0
369;6EGM;relative of another de novo design;DE NOVO PROTEIN             ;20-AUG-18;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;http://dx.doi.org/10.1002/chem.201806040;30861211.0
370;6EGN;relative of another de novo design;DE NOVO PROTEIN             ;20-AUG-18;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;http://dx.doi.org/10.1002/chem.201806040;30861211.0
371;6EGO;relative of another de novo design;DE NOVO PROTEIN             ;20-AUG-18;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;http://dx.doi.org/10.1002/chem.201806040;30861211.0
372;6EGP;relative of another de novo design;DE NOVO PROTEIN             ;20-AUG-18;L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                                 ;;
373;6EIK;relative of another de novo design;DE NOVO PROTEIN             ;19-SEP-17;G.G.RHYS,A.J.BURTON,W.M.DAWSON,F.THOMAS,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/acssynbio.8b00225;29944338.0
374;6EIZ;original de novo design;DE NOVO PROTEIN             ;19-SEP-17;G.G.RHYS,A.J.BURTON,W.M.DAWSON,F.THOMAS,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/acssynbio.8b00225;29944338.0
375;6FES;original de novo design;DE NOVO PROTEIN             ;03-JAN-18;M.ELGAMACY,M.COLES,P.ERNST,H.ZHU,M.D.HARTMANN,A.PLUECKTHUN,A.LUPAS   ;http://dx.doi.org/10.1021/acssynbio.8b00224;30148951.0
376;6FF6;original de novo design;DE NOVO PROTEIN             ;03-JAN-18;M.ELGAMACY,M.COLES,P.ERNST,H.ZHU,M.D.HARTMANN,A.PLUECKTHUN,A.N.LUPAS ;http://dx.doi.org/10.1021/acssynbio.8b00224;30148951.0
377;6G65;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
378;6G66;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
379;6G67;original de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
380;6G68;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
381;6G69;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
382;6G6A;original de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
383;6G6B;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
384;6G6C;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
385;6G6D;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
386;6G6E;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
387;6G6F;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
388;6G6G;relative of another de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
389;6G6H;original de novo design;DE NOVO PROTEIN             ;01-APR-18;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-018-06391-y;30297707.0
390;6H5H;original de novo design;UNKNOWN FUNCTION            ;24-JUL-18;M.ELGAMACY,M.COLES,A.N.LUPAS                                         ;http://dx.doi.org/10.1016/j.jsb.2018.10.010;30558718.0
391;6HQE;original de novo design;PROTEIN FIBRIL             ;24-SEP-18;T.OSINSKI,F.WANG,S.A.HUGHES,M.A.B.KREUTZBERGER,V.P.CONTICELLO,        E.H.EGELMAN                                                         ;http://dx.doi.org/10.1073/pnas.1903910116;31262809.0
392;6MK1;original de novo design;PROTEIN FIBRIL             ;24-SEP-18;F.WANG,S.A.HUGHES,A.ORLOVA,V.P.CONTICELLO,E.H.EGELMAN                ;http://dx.doi.org/10.1073/pnas.1903910116;31262809.0
393;6I1J;relative of another de novo design;DE NOVO PROTEIN             ;28-OCT-18;A.L.BOYLE,N.S.PANNU                                                  ;http://dx.doi.org/10.1039/c9sc01165j;31489168.0
394;6IWB;original de novo design;APOPTOSIS                ;05-DEC-18;S.KIM,M.J.KWAK,B.-H.OH,B.E.CORREIA,P.GAINZA                          ;http://dx.doi.org/10.1038/s41587-019-0403-9;32015549.0
395;6M6Z;original de novo design;DE NOVO PROTEIN             ;16-MAR-20;P.LU,C.XU,G.REGGIANO,Q.XU,F.DIMAIO,D.BAKER                           ;http://dx.doi.org/10.1038/s41586-020-2646-5;32848250.0
396;6O35;original de novo design;DE NOVO PROTEIN             ;25-FEB-19;M.J.BICK,C.XU,B.SANKARAN,D.BAKER                                     ;http://dx.doi.org/10.1038/s41586-020-2646-5;32848250.0
397;6TJ1;original de novo design;BIOSYNTHETIC PROTEIN          ;23-NOV-19;C.XU,X.Y.PEI,B.F.LUISI,D.BAKER                                       ;http://dx.doi.org/10.1038/s41586-020-2646-5;32848250.0
398;6TMS;original de novo design;BIOSYNTHETIC PROTEIN          ;05-DEC-19;C.XU,X.Y.PEI,B.F.LUISI,D.BAKER                                       ;http://dx.doi.org/10.1038/s41586-020-2646-5;32848250.0
399;6MCD;relative of another de novo design;DE NOVO PROTEIN             ;31-AUG-18;A.E.TOLBERT,L.RUCKTHONG,J.A.STUCKEY,V.L.PECORARO                     ;http://dx.doi.org/10.1038/s41557-020-0423-6;32123337.0
400;6MCT;original de novo design;DE NOVO PROTEIN             ;02-SEP-18;M.MRAVIC,L.LIU,W.F.DEGRADO                                           ;http://dx.doi.org/10.1126/science.aav7541;30923216.0
401;6MRR;original de novo design;DE NOVO PROTEIN             ;15-OCT-18;B.KOEPNICK,M.J.BICK,R.D.ESTEP,S.KLEINFELTER,L.WEI,D.BAKER            ;http://dx.doi.org/10.1038/s41586-019-1274-4;31168091.0
402;6MRS;original de novo design;DE NOVO PROTEIN             ;15-OCT-18;B.KOEPNICK,A.BOYKOV,D.BAKER                                          ;http://dx.doi.org/10.1038/s41586-019-1274-4;31168091.0
403;6MSP;original de novo design;DE NOVO PROTEIN             ;17-OCT-18;G.LIU,Y.ISHIDA,G.V.T.SWAPNA,S.KLEINFELTER,B.KOEPNICK,D.BAKER,         G.T.MONTELIONE                                                      ;http://dx.doi.org/10.1038/s41586-019-1274-4;31168091.0
404;6NUK;original de novo design;DE NOVO PROTEIN             ;01-FEB-19;B.KOEPNICK,M.J.BICK,F.DIMAIO,T.NORGARD-SOLANO,D.BAKER                ;http://dx.doi.org/10.1038/s41586-019-1274-4;31168091.0
405;6W15;original de novo design;TRANSFERASE               ;03-MAR-20;SEATTLE STRUCTURAL GENOMICS CENTER FOR INFECTIOUS DISEASE,SEATTLE     STRUCTURAL GENOMICS CENTER FOR INFECTIOUS DISEASE (SSGCID)          ;;
406;6MPW;relative of another de novo design;DE NOVO PROTEIN             ;08-OCT-18;M.MRAVIC,L.LIU,W.F.DEGRADO                                           ;http://dx.doi.org/10.1126/science.aav7541;30923216.0
407;6MQ2;relative of another de novo design;DE NOVO PROTEIN             ;09-OCT-18;M.MRAVIC,L.LIU,W.F.DEGRADO                                           ;http://dx.doi.org/10.1126/science.aav7541;30923216.0
408;6MQU;relative of another de novo design;DE NOVO PROTEIN             ;10-OCT-18;M.MRAVIC,J.L.THOMASTON,W.F.DEGRADO                                   ;http://dx.doi.org/10.1126/science.aav7541;30923216.0
409;6O3N;relative of another de novo design;DE NOVO PROTEIN             ;26-FEB-19;S.-Q.ZHANG,L.LIU,W.F.DEGRADO                                         ;http://dx.doi.org/10.1038/s41589-018-0105-5;30061717.0
410;6OLN;relative of another de novo design;DE NOVO PROTEIN             ;16-APR-19;K.A.SCHEIB,W.S.HORNE                                                 ;http://dx.doi.org/10.1039/c9cc03496j;31204733.0
411;6OLO;relative of another de novo design;DE NOVO PROTEIN             ;16-APR-19;K.A.SCHIEB,W.S.HORNE                                                 ;http://dx.doi.org/10.1039/c9cc03496j;31204733.0
412;6OS8;relative of another de novo design;DE NOVO PROTEIN             ;01-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
413;6OSD;relative of another de novo design;DE NOVO PROTEIN             ;01-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
414;6OV9;relative of another de novo design;DE NOVO PROTEIN             ;07-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
415;6OVS;relative of another de novo design;DE NOVO PROTEIN             ;08-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
416;6OVU;relative of another de novo design;DE NOVO PROTEIN             ;08-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
417;6OVV;relative of another de novo design;DE NOVO PROTEIN             ;08-MAY-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
418;6OWD;relative of another de novo design;DE NOVO PROTEIN             ;09-MAY-19;N.A.BIOK,C.A.BINGMAN,S.H.GELLMAN                                     ;http://dx.doi.org/10.1021/acs.biochem.9b00668;31738525.0
419;6RX5;engineered;OXIDOREDUCTASE             ;07-JUN-19;G.LANDI,C.POZZI,S.MANGANI                                            ;;
420;6R60;engineered;UNKNOWN FUNCTION            ;26-MAR-19;E.AFANASIEVA,A.N.LUPAS,M.D.HARTMANN                                  ;http://dx.doi.org/10.7554/eLife.49853;31613220.0
421;6R5Z;engineered;UNKNOWN FUNCTION            ;26-MAR-19;J.MARTIN,A.N.LUPAS,M.D.HARTMANN                                      ;http://dx.doi.org/10.7554/eLife.49853;31613220.0
422;6R5Y;engineered;UNKNOWN FUNCTION            ;25-MAR-19;J.MARTIN,A.N.LUPAS,M.D.HARTMANN                                      ;http://dx.doi.org/10.7554/eLife.49853;31613220.0
423;6Q1W;relative of another de novo design;DE NOVO PROTEIN             ;06-AUG-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
424;6Q22;relative of another de novo design;DE NOVO PROTEIN             ;06-AUG-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
425;6Q25;relative of another de novo design;DE NOVO PROTEIN             ;06-AUG-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
426;6Q5H;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;C.W.WOOD,J.L.BEESLEY,G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
427;6Q5I;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
428;6Q5J;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
429;6Q5K;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;C.W.WOOD,J.L.BEESLEY,G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
430;6Q5L;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
431;6Q5M;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;C.W.WOOD,J.L.BEESLEY,G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
432;6Q5N;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;C.W.WOOD,J.L.BEESLEY,G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
433;6Q5O;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
434;6Q5P;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
435;6Q5Q;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;N.R.ZACCAI,G.G.RHYS,C.W.WOOD,J.L.BEESLEY,R.L.BRADY,D.N.WOOLFSON      ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
436;6Q5R;relative of another de novo design;DE NOVO PROTEIN             ;09-DEC-18;J.L.BEESLEY,G.G.RHYS,C.W.WOOD,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
437;6Q5S;original de novo design;DE NOVO PROTEIN             ;09-DEC-18;J.L.BEESLEY,G.R.GUTO,C.W.WOOD,R.L.BRADY,D.N.WOOLFSON                 ;http://dx.doi.org/10.1021/jacs.8b13354;31066556.0
438;6U47;relative of another de novo design;DE NOVO PROTEIN             ;23-AUG-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
439;6UCX;small, non-systematic, and other;DE NOVO PROTEIN             ;18-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
440;6UD9;small, non-systematic, and other;DE NOVO PROTEIN             ;19-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
441;6UDR;small, non-systematic, and other;DE NOVO PROTEIN             ;19-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
442;6UDW;small, non-systematic, and other;DE NOVO PROTEIN             ;19-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
443;6UDZ;small, non-systematic, and other;DE NOVO PROTEIN             ;20-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
444;6UF4;small, non-systematic, and other;DE NOVO PROTEIN             ;23-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
445;6UF7;small, non-systematic, and other;DE NOVO PROTEIN             ;23-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
446;6UF8;small, non-systematic, and other;DE NOVO PROTEIN             ;23-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
447;6UF9;small, non-systematic, and other;DE NOVO PROTEIN             ;24-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
448;6UFA;small, non-systematic, and other;DE NOVO PROTEIN             ;24-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
449;6UFU;small, non-systematic, and other;DE NOVO PROTEIN             ;25-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
450;6UG2;small, non-systematic, and other;DE NOVO PROTEIN             ;25-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
451;6UG3;small, non-systematic, and other;DE NOVO PROTEIN             ;25-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
452;6UG6;small, non-systematic, and other;DE NOVO PROTEIN             ;25-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
453;6UGB;small, non-systematic, and other;DE NOVO PROTEIN             ;26-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
454;6UGC;small, non-systematic, and other;DE NOVO PROTEIN             ;26-SEP-19;V.K.MULLIGAN,C.S.KANG,I.ANTSELOVICH,M.R.SAWAYA,T.O.YEATES,D.BAKER    ;http://dx.doi.org/10.1002/pro.3974;33058266.0
455;6UXS;small, non-systematic, and other;TRANSCRIPTION              ;08-NOV-19;P.D.SOLOMON                                                          ;;
456;6V4Y;relative of another de novo design;DE NOVO PROTEIN             ;02-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
457;6V50;relative of another de novo design;DE NOVO PROTEIN             ;02-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
458;6V57;relative of another de novo design;DE NOVO PROTEIN             ;03-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
459;6V58;relative of another de novo design;DE NOVO PROTEIN             ;03-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
460;6V5G;relative of another de novo design;DE NOVO PROTEIN             ;04-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
461;6V5I;relative of another de novo design;DE NOVO PROTEIN             ;04-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
462;6V5J;relative of another de novo design;DE NOVO PROTEIN             ;04-DEC-19;M.S.SMITH,K.L.STERN,W.M.BILLINGS,J.L.PRICE                           ;http://dx.doi.org/10.1021/acs.biochem.0c00045;32270676.0
463;6VZX;relative of another de novo design;BIOSYNTHETIC PROTEIN          ;28-FEB-20;M.D.MILLER,S.A.HULGAN,W.XU,A.J.KOSGEI,G.N.PHILLIPS JR.,J.D.HARTGERINK;http://dx.doi.org/10.1021/acs.biomac.0c00878;32820897.0
464;6W46;relative of another de novo design;BIOSYNTHETIC PROTEIN          ;10-MAR-20;D.M.CHENOWETH,S.D.MELTON                                             ;http://dx.doi.org/10.1039/d0sc03003a;34094319.0
465;6W47;relative of another de novo design;BIOSYNTHETIC PROTEIN          ;10-MAR-20;D.M.CHENOWETH,S.D.MELTON                                             ;http://dx.doi.org/10.1039/d0sc03003a;34094319.0
466;6W70;original de novo design;DE NOVO PROTEIN             ;18-MAR-20;N.F.POLIZZI                                                          ;http://dx.doi.org/10.1126/science.abb8330;32883865.0
467;6W6X;original de novo design;DE NOVO PROTEIN             ;18-MAR-20;N.F.POLIZZI                                                          ;http://dx.doi.org/10.1126/science.abb8330;32883865.0
468;6X8N;original de novo design;DE NOVO PROTEIN             ;01-JUN-20;N.F.POLIZZI                                                          ;http://dx.doi.org/10.1126/science.abb8330;32883865.0
469;6WI5;original de novo design;DE NOVO PROTEIN             ;08-APR-20;A.K.BERA,B.KOEPNICK,A.BOYKOV,D.BAKER                                 ;;
470;6WKY;relative of another de novo design;PROTEIN FIBRIL             ;17-APR-20;F.WANG,O.M.GNEWOU,E.H.EGELMAN,V.P.CONTICELLO                         ;http://dx.doi.org/10.1038/s41467-020-20689-w;33462223.0
471;6WL0;relative of another de novo design;PROTEIN FIBRIL             ;17-APR-20;F.WANG,O.M.GNEWOU,Z.SU,E.H.EGELMAN,V.P.CONTICELLO                    ;http://dx.doi.org/10.1038/s41467-020-20689-w;33462223.0
472;6WL1;original de novo design;PROTEIN FIBRIL             ;17-APR-20;F.WANG,O.M.GNEWOU,C.MODLIN,E.H.EGELMAN,V.P.CONTICELLO                ;http://dx.doi.org/10.1038/s41467-020-20689-w;33462223.0
473;6X9Z;original de novo design;DE NOVO PROTEIN             ;03-JUN-20;A.K.BERA,A.A.VOROBIEVA,A.S.KANG,D.BAKER                              ;http://dx.doi.org/10.1126/science.abc8182;33602829.0
474;6X1K;original de novo design;MEMBRANE PROTEIN            ;19-MAY-20;B.LIANG,A.A.VOROBIEVA,C.M.CHOW,D.BAKER,L.K.TAMM                      ;http://dx.doi.org/10.1126/science.abc8182;33602829.0
475;6XXZ;relative of another de novo design;DE NOVO PROTEIN             ;29-JAN-20;C.L.EDGELL,N.J.SAVERY,D.N.WOOLFSON                                   ;http://dx.doi.org/10.1021/acs.biochem.0c00082;32133841.0
476;6XY0;relative of another de novo design;DE NOVO PROTEIN             ;29-JAN-20;C.L.EDGELL,N.J.SAVERY,D.N.WOOLFSON                                   ;http://dx.doi.org/10.1021/acs.biochem.0c00082;32133841.0
477;6XY1;original de novo design;DE NOVO PROTEIN             ;29-JAN-20;C.L.EDGELL,N.J.SAVERY,D.N.WOOLFSON                                   ;http://dx.doi.org/10.1021/acs.biochem.0c00082;32133841.0
478;6XXV;original de novo design;IMMUNE SYSTEM              ;28-JAN-20;C.YANG,F.SESTERHENN,B.E.CORREIA,F.POJER                              ;http://dx.doi.org/10.1126/science.aay5051;32409444.0
479;6YWC;original de novo design;DE NOVO PROTEIN             ;29-APR-20;C.YANG,F.SESTERHENN,F.POJER,B.E.CORREIA                              ;http://dx.doi.org/10.1038/s41589-020-00699-x;33398169.0
480;6YWD;original de novo design;DE NOVO PROTEIN             ;29-APR-20;C.YANG,F.SESTERHENN,F.POJER,B.E.CORREIA                              ;http://dx.doi.org/10.1038/s41589-020-00699-x;33398169.0
481;6ZT1;original de novo design;DE NOVO PROTEIN             ;17-JUL-20;G.G.RHYS,R.L.BRADY,D.N.WOOLFSON                                      ;http://dx.doi.org/10.1038/s41467-021-21851-8;33750792.0
482;7JH5;original de novo design;DE NOVO PROTEIN             ;20-JUL-20;M.J.BICK,M.J.LAJOIE,S.E.BOYKEN,B.SANKARAN,D.BAKER                    ;http://dx.doi.org/10.1126/science.aba6527;32820060.0
"""