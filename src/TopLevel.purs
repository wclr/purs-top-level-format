module TopLevel
  (formatDefault, adjustIndentDefault, defaultRules, format, main
  ) where


import Prelude

import Data.Array (any, foldl, length, replicate, take, unsnoc)
import Data.Array as Array
import Data.FoldableWithIndex (findWithIndex)
import Data.Int (even)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String (Pattern(..), split, stripPrefix, stripSuffix)
import Data.String as S
import Data.String.CodeUnits (countPrefix)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (log)


type FormatRules =
  { linesAfterModule :: Int
  , linesAfterImports :: Int
  , linesBetweenBlocks :: Int
  , linesBeforeSingleComment :: Int
  , linesAfterSingleComment :: Int
  }


defaultRules :: FormatRules
defaultRules =
  { linesAfterModule: 2
  , linesAfterImports: 2
  , linesBetweenBlocks: 2
  , linesBeforeSingleComment: 2
  , linesAfterSingleComment: 2
  }


data CodeLine
  = Module String
  | ImportOpen String
  | Import String
  | SingleLineComment String
  | MultiLineCommentSingle String
  | MultiLineCommentStart String
  | MultiLineCommentEnd String
  | Type String
  | Decl String
  | Foreign String
  | Fun String
  | NonTop String
  | Empty


derive instance eqCodeLine :: Eq CodeLine


getCodeLineStr :: CodeLine -> String
getCodeLineStr codeL =
  case codeL of
    Module str -> str
    ImportOpen str -> str
    Import str -> str
    SingleLineComment str -> str
    MultiLineCommentSingle str -> str
    MultiLineCommentStart str -> str
    MultiLineCommentEnd str -> str
    Type str -> str
    Decl str -> str
    Foreign str -> str
    Fun str -> str
    NonTop str -> str
    Empty -> ""


startsWith :: String -> String -> Boolean
startsWith prefix =
  isJust <<< stripPrefix (Pattern prefix)


endsWith :: String -> String -> Boolean
endsWith suffix =
  isJust <<< stripSuffix (Pattern suffix)


startsWithAny :: Array String -> String -> Boolean
startsWithAny opts str =
  any (flip startsWith str) opts


trimEnd :: String -> String
trimEnd = R.replace re ""
  where re = unsafeRegex "\\s+$" noFlags


typeKeywords :: Array String
typeKeywords =
  flip append " " <$>
      [ "data"
      , "newtype"
      , "type"
      , "class"
      ]


declKeywords :: Array String
declKeywords =
  flip append " " <$>
      [ "derive"
      , "instance"
      , "infixr"
      , "infixl"
      , "foreign"
      ]


getCodeLine :: String -> CodeLine
getCodeLine str
  | eq "" str = Empty
  | startsWith "--" str = SingleLineComment str
  | startsWith "{-" str && endsWith "-}" str = MultiLineCommentSingle str
  | startsWith "{-" str = MultiLineCommentStart str
  | endsWith "-}" str = MultiLineCommentEnd str
  | startsWith "module " str = Module str
  | startsWith "import " str && S.contains (Pattern " as ") str = Import str
  | startsWith "import " str
      && S.contains (Pattern " (") str
      && not S.contains (Pattern " hiding") str = Import str
  | startsWith "import " str = ImportOpen str
  | startsWithAny typeKeywords str = Type str
  | startsWithAny declKeywords str = Decl str
  | startsWith "foreign" str = Foreign str
  | not startsWith " " str = Fun str
  | otherwise = NonTop str


isFunTypeDecl :: CodeLine -> Boolean
isFunTypeDecl line =
  case line of
    Fun str ->
      R.test (unsafeRegex "^[\\S]+\\s?(::|∷)" noFlags) str

    _ ->
      false


type FormatState =
  { result :: Array String
  , hasImportOpen :: Boolean
  , hasImport :: Boolean
  , isCommented :: Boolean
  , isMultiLineStr :: Boolean
  , prevTopIdentifier :: String
  , prevLine :: CodeLine
  , blanks :: Int
  , nonTops :: Int
  , index :: Int
  , codeLines :: Array CodeLine
  }


initState :: Array CodeLine -> FormatState
initState codeLines =
  { result: []
  , index: 0
  , blanks: 0
  , nonTops: 0
  , prevLine: Empty
  , prevTopIdentifier: ""
  , hasImportOpen: false
  , hasImport: false
  , isCommented: false
  , isMultiLineStr: false
  , codeLines
  }


getTopIdentifier :: CodeLine -> Maybe String
getTopIdentifier cl = case cl of
  Fun str -> Just $ S.joinWith " " $ take 1 (S.split (Pattern " ") str)
  Type str -> Just $ S.joinWith " " $ take 2 (S.split (Pattern " ") str)
  _ -> Nothing


isFun :: CodeLine -> Boolean
isFun =
  case _ of
    Fun _ -> true
    _ -> false


isCommentEnd :: CodeLine -> Boolean
isCommentEnd cl = case cl of
  MultiLineCommentEnd _ -> true
  MultiLineCommentSingle _ -> true
  _ -> false


isMultiCommentEnd :: CodeLine -> Boolean
isMultiCommentEnd cl = case cl of
  MultiLineCommentEnd _ -> true
  _ -> false


isSingleComment :: CodeLine -> Boolean
isSingleComment cl = case cl of
  SingleLineComment _ -> true
  _ -> false


isQualifiedImport :: CodeLine -> Boolean
isQualifiedImport cl = case cl of
  Import _ -> true
  _ -> false


isOpenImport :: CodeLine -> Boolean
isOpenImport cl = case cl of
  ImportOpen _ -> true
  _ -> false


{- Should contain odd number of triple quotes.  -}
hasMultiLineQuotes :: String -> Boolean
hasMultiLineQuotes =
  even <<< length <<< (split $ Pattern "\"\"\"")


spaces :: Int -> String
spaces n = S.joinWith "" $ replicate n " "
getNextNonMultiCommentLine :: FormatState -> Maybe CodeLine
getNextNonMultiCommentLine { codeLines, index } =
  join $ commentEnd <#> (\found -> Array.index codeLines (found.index + 1) )
  where
  commentEnd =
    (flip findWithIndex)
      codeLines
        \i codeL -> i > index && (isMultiCommentEnd codeL)


getNextNonCommentLine :: FormatState -> Maybe CodeLine
getNextNonCommentLine { codeLines, index } =
  res <#> (\found -> found.value)
  where
  res =
    (flip findWithIndex)
      codeLines
        \i codeL -> i > index && not (isSingleComment codeL)


isNextNonCommentLineEmpty :: FormatState -> Boolean
isNextNonCommentLineEmpty st =
  case getNextNonCommentLine st of
    Nothing -> true
    Just Empty -> true
    _ -> false


isNextNonMultiCommentLineEmpty :: FormatState -> Boolean
isNextNonMultiCommentLineEmpty st =
  case getNextNonMultiCommentLine st of
    Nothing -> true
    Just Empty -> true
    _ -> false


getNextLine :: FormatState -> Maybe CodeLine
getNextLine { codeLines, index } =
  Array.index codeLines (index + 1)



-- isNextLineEmpty :: FormatState -> Boolean
-- isNextLineEmpty state = case getNextLine state of
--   Just codeL -> codeL == Empty
--   Nothing -> true


foldFormat :: FormatState -> CodeLine -> FormatState
foldFormat state codeL =
  if codeL == Empty then
    state { blanks = state.blanks + 1, index = nextIndex }
  else
    let
      { prevLine } = state
      topId = getTopIdentifier codeL
      -- sameId means previous was type annotation
      sameId = topId == Just state.prevTopIdentifier
      isPrevOneLine = state.blanks + state.nonTops == 0
      isPrevLineSingleComment = isSingleComment prevLine
      isPrevLineNearbyComment = isPrevLineSingleComment && state.blanks == 0
      isPrevCommentEnd = \_ -> isPrevLineNearbyComment || isCommentEnd state.prevLine
      isPrevImport = isQualifiedImport prevLine || isOpenImport prevLine
      isPrevEmpty = (prevLine == Empty)

      commentAdd = if isPrevImport then max 2 (min 3 blanks) else 2

      addBefore =
        if state.isCommented || state.isMultiLineStr then
          state.blanks
        else case codeL of
          Module _ -> 0

          ImportOpen _ ->
            if state.hasImportOpen then 0 else 2

          Import _ -> if state.hasImport || isPrevCommentEnd unit
            then 0
            else if state.hasImportOpen then 1 else 2

          MultiLineCommentSingle _ ->
            if isPrevEmpty then 0 else commentAdd

          SingleLineComment _
            | isPrevOneLine -> 0

            -- for orphan comments line force 3 lines above
            | (blanks > 0 && isNextNonCommentLineEmpty state) -> 3

            -- allow close comments without spacing
            | (blanks == 0 && isNextNonCommentLineEmpty state) -> 0

            | otherwise -> commentAdd

          MultiLineCommentStart _
            | isPrevEmpty -> 0
            | (blanks > 0 && isNextNonMultiCommentLineEmpty state) -> 3
            -- | isPrevImport -> state.blanks
            | otherwise -> 2

          Type _ ->
            if sameId || isPrevOneLine || isPrevCommentEnd unit then 0 else 2

          Decl _ ->
            if isPrevOneLine || isPrevCommentEnd unit then 0 else 2

          Fun _ ->
            if sameId
              -- allow squash one line value decl (e.g., constants)
              || (blanks == 0 && isFun prevLine && not isFunTypeDecl codeL)
              || (isPrevCommentEnd unit && blanks == 0) then
              0
            else
              2

          NonTop _ ->
            min blanks 1

          Empty -> 0
          _ -> 0
    in
      state
        { result =
          state.result <> (replicate addBefore "\n") <> [ str <> "\n" ]
        , index = nextIndex
        , blanks = 0
        , nonTops = if
            codeL == NonTop str then state.nonTops + 1 else 0
        , prevLine = codeL
        , prevTopIdentifier = fromMaybe state.prevTopIdentifier topId
        , hasImportOpen = state.hasImportOpen || codeL == ImportOpen str
        , hasImport = state.hasImport || codeL == Import str
        , isCommented = case codeL of
            MultiLineCommentStart _ -> true
            MultiLineCommentEnd _ -> false
            _ -> state.isCommented
        , isMultiLineStr =
          if state.isCommented then
            state.isMultiLineStr
          else if hasMultiLineQuotes str then
            not state.isMultiLineStr
          else
            state.isMultiLineStr
        }
    where
      nextIndex = state.index + 1
      str = getCodeLineStr codeL
      blanks = state.blanks
      --codeL = getCodeLine str



-- INDENTATION ADJUSTMENT (NOT IMPLEMENTED)


type IndentOptions
  = {}


indentDefaultOptions :: IndentOptions
indentDefaultOptions = {}


type AdjustIndentState =
  { result :: Array String
  , prevOrgIndent :: Int
  , prevResultIndent :: Int
  , bracketStack :: Array { indent :: Int, resultIndent :: Int }
  }


targetIndent :: Int
targetIndent = 2
closeToOpenPairs :: Map String String
closeToOpenPairs = Map.empty
  # Map.insert "]" "["
  # Map.insert "}" "{"


openSymbols :: Array String
openSymbols = List.toUnfoldable $ Map.values closeToOpenPairs
closeSymbols :: Array String
closeSymbols = Set.toUnfoldable $ Map.keys closeToOpenPairs
foldAdjustIndent :: AdjustIndentState -> String -> AdjustIndentState
foldAdjustIndent state str =
  state
    { result = state.result <> [spaces resultIndent <> strWithoutIndent]
    , prevOrgIndent = indent
    , prevResultIndent = resultIndent
    , bracketStack =
      if isOpenBracket then
        state.bracketStack <> [ { indent, resultIndent } ]
      else case stack of
        Nothing -> state.bracketStack
        Just { init } -> init
    }
  where
  indent = countPrefix (eq ' ') str
  strWithoutIndent = S.drop indent str

  isOpenBracket = strWithoutIndent # startsWith "["
  isCloseBracket = strWithoutIndent # startsWith "]"

  stack = if isCloseBracket
    then unsnoc state.bracketStack
    else Nothing

  prevOrgIndentDiff = indent - state.prevOrgIndent
  prevOrgNoDiff = prevOrgIndentDiff == 0

  prevResultIndentDiff = indent - state.prevResultIndent

  resultIndent =
    case stack of
      Just { last } -> last.resultIndent
      Nothing ->
        if prevResultIndentDiff > targetIndent
        then state.prevResultIndent + targetIndent
        else
          if prevOrgNoDiff
          then state.prevResultIndent
          else indent


initAdjustState :: AdjustIndentState
initAdjustState =
  { result: []
  , prevOrgIndent: 0
  , prevResultIndent: 0
  , bracketStack: []
  }



-- API


adjustIndent :: IndentOptions -> String -> String
adjustIndent opts text = S.joinWith "\n" state.result
  where
  lines = trimEnd <$> S.split (Pattern "\n") text
  state = foldl foldAdjustIndent initAdjustState lines


adjustIndentDefault :: String -> String
adjustIndentDefault = adjustIndent indentDefaultOptions
format :: FormatRules -> String -> String
format opts text = S.joinWith "" state.result
  where
  lines = trimEnd <$> S.split (Pattern "\n") text
  codeLines = lines <#> getCodeLine
  state = foldl foldFormat (initState codeLines) codeLines


formatDefault :: String -> String
formatDefault = format defaultRules
main :: Effect Unit
main = do
  log $ "Top Level Format"
