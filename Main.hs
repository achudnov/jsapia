module Main where

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Data (Data)
--import System.IO
import Data.Generics.Uniplate.Data
import Data.List

main :: IO ()
main = getContents >>= parseJavaScript >>=
       (print . apiAnalysis)

parseJavaScript :: String -> IO (JavaScript SourcePos)
parseJavaScript source = case parse parseScript "stdin" source of
  Left perr -> fail $ show perr
  Right js  -> return js

data APIAnalysisResults = APIAnalysisResults {apiElements :: Set APIElement
                                             ,warnings    :: [Warning]}
                          
instance Show APIAnalysisResults where
  show r = "The following API methods and fields are used: " ++ 
           intercalate ", " (map show $ Set.toList $ apiElements r) ++
           ".\n" ++ if null $ warnings r then "No warnings to report"
                    else "Warning, the analysis results might not be sound: " ++
                          intercalate "\n" (map show $ warnings r)

data APIElement = APIElement [String]
                deriving (Eq, Ord)
data Warning = Warning SourcePos String

instance Show APIElement where
  show (APIElement elems) = intercalate "." elems
  
instance Show Warning where
  show (Warning pos msg) = show pos ++ ": " ++ msg

topLevelAPIVars = ["window", "document", "XMLHttpRequest", "Object"
                  ,"Function", "Array", "String", "Boolean", "Number", "Date"
                  ,"RegExp", "Error", "EvalError", "RangeError"
                  ,"ReferenceError", "SyntaxError", "TypeError", "URIError"
                  ,"Math", "JSON"]

-- | The API analysis function. Returns a 'Set' of API
-- functions/fields and a list of warnings
apiAnalysis :: JavaScript SourcePos -> APIAnalysisResults
apiAnalysis script = APIAnalysisResults (Set.fromList $ genApiElements script)
                                        (genWarnings script)

genApiElements :: JavaScript SourcePos -> [APIElement]
genApiElements = map toAPIElement . filter isAnAPIAccessor . universeBi 
  where isAnAPIAccessor :: Expression SourcePos -> Bool
        isAnAPIAccessor e = case e of
          DotRef _ o _      -> isAnAPIAccessor o
          BracketRef _ o _  -> isAnAPIAccessor o
          VarRef _ (Id _ v) -> v `elem` topLevelAPIVars
          _                 -> False
        toAPIElement e = case e of
          DotRef _ o (Id _ fld) -> toAPIElement o `addElement` fld
          BracketRef _ o (StringLit _ s) -> toAPIElement o `addElement` s
          VarRef _ (Id _ vn) -> APIElement [vn]
          _                  -> APIElement []
        addElement :: APIElement -> String -> APIElement
        addElement (APIElement elems) elem = APIElement (elems ++ [elem])
        
genWarnings :: JavaScript SourcePos -> [Warning]
genWarnings script = concatMap warnAliased topLevelAPIVars
                   ++warnEvalUsed
  where warnAliased varName =
          [Warning (getAnnotation e) $ varName ++ " is aliased (in " ++ 
           renderExpression e ++ ")"
          |e@(AssignExpr _ _ _ (VarRef _ (Id _ vn))) <- universeBi script,
                                vn == varName]++
          [Warning (getAnnotation vd) $ varName ++ " is aliased (in " ++
           show vd ++ ")"
          |vd@(VarDecl _ _ (Just (VarRef _ (Id _ vn)))) <- universeBi script,
           vn == varName]
        warnEvalUsed = [Warning (getAnnotation e) "Eval used"
                       |e@(CallExpr _ f _) <- universeBi script,
                        isEvalRef f]
        isEvalRef e = case e of
          VarRef _ (Id _ "eval") -> True
          DotRef _ (VarRef _ (Id _ "window")) (Id _ "eval") -> True
          _ -> False
          
-- | returns true if inside the expression there is a reference to the
-- variable
usesVariable :: (Data a) => String -> Expression a -> Bool
usesVariable v e = and [v' == v | VarRef _ (Id _ v') <- universe e]
