
{-# LANGUAGE OverloadedStrings #-}

module Utils.Concierge where

import Control.Applicative((<$>))
import Control.Monad(foldM,unless)
import Data.Map hiding (map,foldl,filter)
import Data.String
import Database.PostgreSQL.Simple
import Prelude hiding (lookup)

data Action = Arrange | Verify | VerifyStrict | Retrofit

readOnly :: Action -> Bool
readOnly Arrange = False
readOnly Verify = True
readOnly VerifyStrict = True
readOnly Retrofit = False

data DependencyKind = Requires deriving Show

toString :: DependencyKind -> String
toString Requires = "requires"

type Dependency = (DependencyKind, Change)

data Change = Change
    {name :: String
    ,ddl :: String
    ,dependencies :: [Dependency]
    }
    deriving Show

change :: String -> [Change -> Change] -> Change
change name = foldl (\c f -> f c) (Change name "" [])

hasDDL :: String -> Change -> Change
hasDDL ddl change = change { ddl = ddl }

requires :: Change -> Change -> Change
requires other = addDependency (Requires, other)

addDependency :: Dependency -> Change -> Change
addDependency dep change = change { dependencies = dep:dependencies change }

maybeIO :: (Monad m) => [m (Maybe x)] -> m (Maybe x)
maybeIO [] = return Nothing
maybeIO (x:xs) = x >>= go
    where
        go Nothing = maybeIO xs
        go r = return r

bootstrap :: Connection -> Action -> IO ()
bootstrap conn act = do
    ensurePatchesTableExists conn act
    mapM_ (arrangeBuiltinChange conn act) builtinSchema
    
ensurePatchesTableExists :: Connection -> Action -> IO ()
ensurePatchesTableExists conn act = do
    [Only ct] <- query_ conn
        "SELECT COUNT(1)::INT FROM pg_tables \
        \WHERE schemaname = 'public' AND tablename = 'concierge_patch'"
    case (ct::Int) of
        0 | readOnly act -> fail "Table 'concierge_patch' doesn't exist"
        0 -> do
            execute_ conn
                "CREATE TABLE concierge_patch( \
                \name VARCHAR PRIMARY KEY NOT NULL \
                \)"
            return ()
        1 -> return ()
        _ -> fail $ "Unexpected number of 'concierge_patch' tables: " ++ show ct

ddlColumn :: Change
ddlColumn = change "concierge_ddl" [
    hasDDL "ALTER TABLE concierge_patch ADD COLUMN ddl VARCHAR NOT NULL"
    ]

dependencyTable :: Change
dependencyTable = change "concierge_dependency" [
    hasDDL
        "CREATE TABLE concierge_dependency( \
        \subject VARCHAR NOT NULL REFERENCES concierge_patch \
        \,verb VARCHAR NOT NULL \
        \,object VARCHAR NOT NULL REFERENCES concierge_patch \
        \,PRIMARY KEY(subject, verb, object) \
        \)"
    ]

builtinSchema :: [Change]
builtinSchema = [ddlColumn, dependencyTable]

arrangeBuiltinChange :: Connection -> Action -> Change -> IO ()
arrangeBuiltinChange conn act change = do
    [Only ct] <- query conn
        "SELECT COUNT(1)::INT FROM concierge_patch WHERE name=?" (Only $ name change)
    case (ct::Int) of
        0 | readOnly act -> fail $ "Patch '" ++ name change ++ "' is missing"
        0 -> do
            execute_ conn (fromString $ ddl change)
            execute conn
                "INSERT INTO concierge_patch(name, ddl) VALUES(?, ?)" (name change, ddl change)
            return ()
        1 -> return ()
        _ -> fail $ "Unexpected number of '" ++ name change ++ "' patches: " ++ show ct

getInstalledPatches :: Connection -> IO [(String, String)]
getInstalledPatches c = nonBuiltin <$> get
    where
        get :: IO [(String, String)]
        get = query_ c "SELECT name, ddl FROM concierge_patch"
        builtins = map name builtinSchema
        nonBuiltin xs = [ (i,j) | (i,j) <- xs, i `notElem` builtins]

dfs :: (Show a, Ord a) => [(a, [a])] -> Either String [a]
dfs xs = go [] $ map fst xs
    where
        go order [] = Right $ reverse order
        go order (x:xs) = case visit order [] x (cache ! x) of
            Left msg -> fail msg
            Right order' -> go order' xs
        visit a _ c _ | c `elem` a = Right a
        visit a [] c [] = Right (c:a)
        visit a [] c (d:ds) = case visit a [c] d (cache ! d) of
            Left msg -> Left msg
            Right a' -> visit a' [] c ds
        visit a b c d = error $ "Not implemented: visit " ++ show (a, b, c, d)
        cache = fromList xs

calculateOrdering :: [Change] -> IO [Change]
calculateOrdering schema = case dfs graph of
        Left msg -> fail msg
        Right result -> return (rebuild result)
    where
        graph = map (\c -> (name c, map (name . snd) $ dependencies c)) schema
        schemaLookup = fromList $ map (\c -> (name c, c)) schema
        rebuild = map (\k -> schemaLookup ! k)

validateSameness :: [Change] -> [(String, String)] -> IO ()
validateSameness schema current = go schema
    where
        cache = fromList current
        go [] = return ()
        go (x:xs) = case lookup (name x) cache of
            Nothing -> go xs
            Just d | d == ddl x -> go xs
            _ -> fail ("DDL mismatch for patch '" ++ name x ++ "'")

pickNew :: [Change] -> [String] -> [Change]
pickNew schema current = filter (\c -> name c `notElem` current) schema

applyPatch :: Connection -> Change -> IO ()
applyPatch conn change = do
    unless (ddl change == "" ) $ do
        execute_ conn (fromString (ddl change))
        return ()
    execute conn
        "INSERT INTO concierge_patch(name, ddl) VALUES (?,?)"
        (name change, ddl change)
    mapM_ (\(dk, other) -> 
            execute conn
                "INSERT INTO concierge_dependency(subject,verb,object) VALUES (?,?,?)"
                (name change, toString dk, name other)
        ) (dependencies change)

arrange :: Connection -> [Change] -> IO ()
arrange c schema = do
    ordering <- calculateOrdering schema
    bootstrap c Arrange
    current <- getInstalledPatches c
    validateSameness schema current
    let toApply = pickNew ordering (map fst current)
    foldM (\() -> applyPatch c) () toApply
