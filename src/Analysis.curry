
module Analysis (splitName,f) where

import Prelude hiding (lookup)
import FlatCurry.Types
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Char (isDigit)
import FlatCurry.Goodies (isVar,isExternal,caseBranches,branchPattern,branchExpr,patCons, funcName, funcBody)
import Data.List (maximumBy, delete, maximum, (\\))
import Data.Maybe (fromJust)
import Data.Map (Map, fromList, insertList, findWithDefault, member, lookup)
import Graph as G


f fs = case id $## nub (map (baseName . funcName) fs) of
        names@(_:_) -> 
         let toInt n = fromJust (lookup n intMap)
             intMap  = fromList (zip names [0..])
             verts   = map toInt names
             fun i   = fromJust (lookup i funMap)
             funMap  = fromList (zip [0..] names)
             edges   = nub [(toInt (baseName (funcName f)), toInt c) | f <- fs,
                                                                       c <- funcCalls f,
                                                                       baseName (funcName f) /= c]
         in zip names verts
--         in case id $## edges of
--              es -> es -- scc verts (take 200 es)

funcCalls :: FuncDecl -> [String]
funcCalls (Func _ _ _ _ (External _)) = []
funcCalls (Func _ _ _ _ (Rule _ b)) = getCalls b
 where 
   getCalls (Var _)        = []
   getCalls (Lit _)        = []
   getCalls (Let vs e)     = mergeMap (getCalls . snd) vs ++- getCalls e
   getCalls (Or e1 e2)     = getCalls e1 ++- getCalls e2
   getCalls (Free _ e)     = getCalls e
   getCalls (Comb ct n es) = if isFunc ct
                             then [baseName  n] ++- mergeMap getCalls es
                             else mergeMap getCalls es
   getCalls (Case _ e bs)  = getCalls e ++- mergeMap (getCalls . branchExpr) bs
   getCalls (Typed e _)    = getCalls e

isFunc :: CombType -> Bool
isFunc FuncCall         = True
isFunc (FuncPartCall _) = True
isFunc ConsCall         = False
isFunc (ConsPartCall _) = False

(++-) :: (Ord a) => [a] -> [a] -> [a]
[]     ++- ys     = ys
(x:xs) ++- []     = x : xs
(x:xs) ++- (y:ys) = case compare x y of
                         LT -> x : (xs ++- (y:ys))
                         EQ -> x : (xs ++- ys)
                         GT -> y : ((x:xs) ++- ys)

mergeMap :: (Ord b) => (a -> [b]) -> [a] -> [b]
mergeMap f = foldr (++-) [] . map f

comps :: [Int] -> [(Int,Int)] -> [([Int], [(Int,Int)])]
comps vs es = map (\i -> ([i],[])) islands ++ map filterEdges components
 where islands = vs \\ (map fst es ++ map snd es)
       components = map G.preorder $ G.components $ G.buildG es
       filterEdges c = (c, filter (\(x,y) -> x `elem` c || y `elem` c) es)

scc :: [Int] -> [(Int,Int)] -> [[Int]]
scc vs es = map (:[]) islands ++ components
 where islands = vs \\ (map fst es ++ map snd es)
       components = map G.preorder $ G.scc $ G.buildG es

type FunName = String
type ClassName = String
type ModName = String
type TypeName = String
data FunType = Prim FunName                    -- prim_...
             | Def FunName ClassName           -- _def#fun#MOD.CLASS
             | Inst ClassName TypeName         -- _inst#MOD.CLASS#MOD.TYPE
             | Impl FunName ClassName TypeName -- _impl#FUN#MOD.CLASS#MOD.TYPE
             | Super ClassName ClassName       -- _super#MOD.DERIVE#MOD.BASE
             | Lambda FunType String           -- FUN._#lambdaSUFFIX
             | Where FunType FunName String    -- FUN._#lambdaSUFFIX
             | Sel FunType String String       -- FUN_#selSUFFIX#VAR
             | Fun FunName
  deriving (Eq, Ord, Show)

splitName :: String -> FunType
splitName name
 | isWhere name                   = readWhere name
 | "._#lambda" `isInfixOf`  name  = readLambda name
 | "_#sel"     `isInfixOf`  name  = readSel name
 | "_def#"     `isPrefixOf` name  = readDef name
 | "_inst#"    `isPrefixOf` name  = readInst name
 | "_impl#"    `isPrefixOf` name  = readImpl name
 | "_super#"   `isPrefixOf` name  = readSuper name
 | "prim_"     `isPrefixOf` name  = Prim (drop 5 name)
 | otherwise                      = Fun name

isWhere name = all isDigit end && not (null end)
  where end = takeWhile (/='.') (reverse name)

readWhere  (fun++"."++name++"."++suffix) | not (elem '.' (name++suffix))      = Where (splitName fun) name suffix
readLambda (fun++"._#lambda"++suffix)    | not (isInfixOf "._#lambda" suffix) = Lambda (splitName fun) suffix
readSel    (fun++"._#sel"++suffix++"#"++var)                    = Sel (splitName fun) suffix var
readInst   ("_inst#"++mod++"."++iclass++"#"++itype)             = Inst iclass (readData itype)
readDef    ("_def#"++fun++"#"++mod++"."++iclass)                = Def fun iclass
readImpl   ("_impl#"++fun++"#"++mod++"."++iclass++"#"++itype)   = Impl fun iclass (readData itype)
readSuper  ("_super#"++mod++"."++iderive++"#"++mod++"."++ibase) = Super iderive ibase

readData "[]" = "[]"
readData "(->)" = "(->)"
readData ("("++commas++")") | all (==',') commas = "("++commas++")"
readData (mod++"."++itype) | not (elem '.' itype) = itype

getBase (Prim f) = "prim_"++f
getBase (Def _ c) = c
getBase (Inst c _) = c
getBase (Impl _ c _) = c
getBase (Super c _) = c
getBase (Lambda f _) = getBase f
getBase (Where f _ _) = getBase f
getBase (Sel f _ _) = getBase f
getBase (Fun f) = f

baseName = getBase . splitName . snd
