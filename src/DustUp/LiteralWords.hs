module DustUp.LiteralWords where

import Data.Bitmask
import Data.Char (toLower)
import Language.Haskell.TH

makeFlagValues :: Name -> TypeQ -> Q [Dec]
makeFlagValues tyName resultTyQ = do
  resultTy <- resultTyQ

  info <- reify tyName

  cons <- case info of
    TyConI (DataD _ _ _ _ cs _) ->
      pure cs
    TyConI (NewtypeD _ _ _ _ c _) ->
      pure [c]
    _ ->
      fail $
        "makeFlagValues expects a data/newtype name, got: "
          ++ show info

  concat <$> traverse (mkFlag resultTy) cons

mkFlag :: Type -> Con -> Q [Dec]
mkFlag resultTy con =
  case getNullaryConName con of
    Nothing ->
      fail $
        "makeFlagValues only supports nullary constructors, got: "
          ++ show con
    Just conName -> do
      let valName = mkName $ map toLower $ nameBase conName
      pure
        [ SigD valName resultTy
        , ValD
            (VarP valName)
            ( NormalB
                ( InfixE
                    (Just (ConE conName))
                    (VarE 'addFlag)
                    (Just (VarE 'noFlag))
                )
            )
            []
        ]

getNullaryConName :: Con -> Maybe Name
getNullaryConName (NormalC n []) = Just n
getNullaryConName (RecC n []) = Just n
getNullaryConName (GadtC [n] [] _) = Just n
getNullaryConName (RecGadtC [n] [] _) = Just n
getNullaryConName _ = Nothing

data With = With
data Certain = Certain
data By = By
type Those = []
data Damage = Damage
data To = To
data From = From
data Onto = Onto
data Life = Life
data Of = Of
data DustSeal = DustSeal

make'action'types :: Name -> Q [Dec]
make'action'types tyName = do
  info <- reify tyName
  cons <- case info of
    TyConI (DataD _ _ _ _ cs _) ->
      pure $ do
        con <- cs
        case con of
          NormalC n _ ->
            pure
              ( NormalC
                  (mkName $ "Action'" ++ nameBase n)
                  []
              , con
              )
          _ -> fail ""
    _ ->
      fail $
        "makeFlagValues expects a data/newtype name, got: "
          ++ show info
  pure
    [ DataD
        []
        (mkName "Action'Types")
        []
        Nothing
        (map fst cons)
        []
    , SigD (mkName "typeof'action") $
        let
          from = AppT actionD andThen
          actionD = ConT $ mkName "ActionD"
          andThen = VarT $ mkName "andThen"
          to = ConT $ mkName "Action'Types"
          ty = AppT (AppT ArrowT from) to
         in
          ty
    , FunD
        (mkName "typeof'action")
        ( do
            (NormalC new [], NormalC old fields) <- cons
            let pats = replicate (length fields) WildP
            pure $
              Clause
                [ConP old [] pats]
                (NormalB $ ConE new)
                []
        )
    ]

make'game'objects :: [Name] -> Name -> Q [Dec]
make'game'objects names game'object = pure $ do
  name <- names
  return $
    TySynD
      (mkName $ nameBase name ++ "O")
      []
      (ConT game'object `AppT` ConT name)
