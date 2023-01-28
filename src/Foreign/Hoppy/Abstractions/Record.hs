{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections #-}
module Foreign.Hoppy.Abstractions.Record (asRecord) where

import qualified Data.List.NonEmpty as NonEmpty
import           Foreign.Hoppy.Runtime (decode)
import           Foreign.Hoppy.Generator.Spec (Class, classIdentifier, classVariables, classVariableType, idPartBase, identifierParts)
import qualified Foreign.Hoppy.Generator.Spec as Hoppy
import           Foreign.Hoppy.Generator.Types (floatT, ptrT)
import           Language.Haskell.TH.Syntax ( Dec (DataD), Exp(AppE, ConE), Name, Q, Type(AppT, ConT), Con (NormalC)
                                            , SourceStrictness (SourceLazy, NoSourceStrictness), Bang (Bang)
                                            , SourceUnpackedness (NoSourceUnpackedness), lookupValueName, newName, lookupTypeName)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import Language.Haskell.TH (ExpQ)

asRecord :: Class -> Q [Dec]
asRecord cls = do
  Just classNameIdent <- pure . fmap NonEmpty.last . NonEmpty.nonEmpty . identifierParts $ classIdentifier cls
  tyName <- newName . (++ "Rec") $ idPartBase classNameIdent
  ctorName <- newName . (++ "Rec") $ idPartBase classNameIdent
  constructorArgs <- mapM (fmap ((Bang NoSourceUnpackedness NoSourceStrictness,) . recordType) . resolveVariableType) variables
  pure [DataD [] tyName [] Nothing [NormalC ctorName constructorArgs] []]

  where
    variables = classVariables cls      

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
  resolveType (classVariableType cppVariable) (pure $ ConE getter)

resolveType cppType baseExpr =
  case cppType of
    Hoppy.Internal_TPtr cppType' -> do
      inner <- resolveType cppType' baseExpr
      readExp <- [| ($(readExp inner) >=> decode) |]
      pure $ inner { readExp = pure readExp }
    native | native == floatT -> do
      recordType <- lookupHoppyType "Float"
      let readExp = baseExpr
      pure VariableTypeSpec {..}
      
