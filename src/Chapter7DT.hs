{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module Chapter7DT where

import Data.Set (Set)
import Lens.Micro.Platform

data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- Clients

data Client
  = GovOrg {_clientName :: String}
  | Company
      { _clientName :: String,
        _person :: Person,
        _duty :: String
      }
  | Individual {_person :: Person}
  deriving stock (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual
  deriving stock (Show, Eq, Ord)

data Person = Person
  { _firstName :: String,
    _lastName :: String,
    _gender :: Gender
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''Client
makeLenses ''Person

-- Products
data Product = Product {productId :: Integer, productType :: ProductType}
  deriving stock (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip
  deriving stock (Show, Eq, Ord)

data Purchase = Purchase {client :: Client, products :: [Product]}
  deriving stock (Show, Eq, Ord)

data PurchaseInfo
  = InfoClientKind ClientKind
  | InfoClientDuty String
  | InfoClientGender Gender
  | InfoPurchasedProduct Integer
  | InfoPurchasedProductType ProductType
  deriving stock (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
  deriving newtype (Eq, Ord)

newtype FrequentSet = FrequentSet (Set PurchaseInfo)
  deriving newtype (Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo)
  deriving stock (Eq, Ord)
