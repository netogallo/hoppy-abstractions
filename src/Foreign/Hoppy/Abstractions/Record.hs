{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections #-}
module Foreign.Hoppy.Abstractions.Record
  ( asRecord
  -- Client helpers
  , (>=>)
  , hoppyDecodeFloat
  ) where

import           Control.Monad ((>=>), foldM)

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)

import           Foreign.Hoppy.Runtime (decode)
import           Foreign.Hoppy.Generator.Spec (Class, classIdentifier, classVariables, classVariableType, idPartBase, identifierParts)
import qualified Foreign.Hoppy.Generator.Spec as Hoppy
import           Foreign.Hoppy.Generator.Types (floatT, ptrT)
import           Foreign.Ptr (Ptr)
import           Foreign.C.Types (CFloat)
import           Language.Haskell.TH (ExpQ, Body (..), nameBase)
import           Language.Haskell.TH.Syntax ( Clause(..), Dec (..), Exp(..), Name, Pat(..), Q, Type(..), Con (NormalC)
                                            , SourceStrictness (SourceLazy, NoSourceStrictness), Bang (Bang)
                                            , SourceUnpackedness (NoSourceUnpackedness), TyVarBndr(..), lookupValueName, newName
                                            , lookupTypeName, reifyType)

asRecord :: Class -> Q [Dec]
asRecord cls = do
  Just classNameIdent <- pure . fmap NonEmpty.last . NonEmpty.nonEmpty . identifierParts $ classIdentifier cls
  tyName <- newName . (++ "Rec") $ idPartBase classNameIdent
  ctorName <- newName . (++ "Rec") $ idPartBase classNameIdent
  constructorTypes <- mapM resolveVariableType variables
  let
    constructorArgs = map ((Bang NoSourceUnpackedness NoSourceStrictness,) . recordType) constructorTypes
  converter <- makeConverter tyName ctorName constructorTypes
  pure $
    [ DataD [] tyName [] Nothing [NormalC ctorName constructorArgs] [] ]
    ++ converter

  where
    variables = classVariables cls      

makeConverter :: Name -> Name -> [VariableTypeSpec] -> Q [Dec]
makeConverter tyName ctor variableSpecs
  | (v : vs) <- variableSpecs = do
      objName <- newName "obj"
      convName <- newName . ("convert_" ++) $ nameBase ctor
      base <- [| $(readExp v) $(pure (VarE objName)) |]
      bodyArgs <- foldM (\s v -> [| $(pure s) <*> $(readExp v) $(pure $ VarE objName) |]) base vs
      body <- [| $(pure $ ConE ctor) <$> $(pure bodyArgs) |]
      Just tcs <- lookupTypeName "AtomInfoTypeValue"
      tvN <- newName "pointer"
      pure
        [ SigD convName (ForallT [PlainTV tvN] [AppT (ConT tcs) (VarT tvN)] (AppT (AppT ArrowT (VarT tvN)) (AppT (ConT ''IO) (ConT tyName))))
        , FunD convName [(Clause [VarP objName]) (NormalB body) []] ]
  | otherwise =
    -- Todo: just call the constructor w/o any arguments
    undefined

lookupHoppyType :: String -> Q Type
lookupHoppyType = fmap (ConT . fromJust) . lookupTypeName

data VariableTypeSpec =
  VariableTypeSpec
  { recordType :: Type
  , readExp :: ExpQ
  }

resolveVariableType :: Hoppy.ClassVariable -> Q VariableTypeSpec
resolveVariableType cppVariable = do
  Just getter <- lookupValueName "anisou_get"
  resolveType (classVariableType cppVariable) (pure $ VarE getter)

hoppyDecodeFloat :: Ptr CFloat -> IO Float
hoppyDecodeFloat = decode

resolveType cppType baseExpr =
  case cppType of
    Hoppy.Internal_TPtr cppType' -> do
      inner <- resolveType cppType' baseExpr
      readExp <- [| ($(readExp inner) >=> hoppyDecodeFloat) |]
      pure $ inner { readExp = pure readExp }

    native | native == floatT -> do
      let recordType =  ConT ''Float
      let readExp = baseExpr
      pure VariableTypeSpec {..}

    _ -> fail $ "type not supported: " ++ show cppType
