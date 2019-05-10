{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Row
import GHC.Generics

type CompanyId = Int

data Company = Company
  { companyName :: String
  } deriving (Show, Eq, Generic)

type ProductId = Int

data Product = Product
  { productName :: String
  , productCompanyId :: CompanyId
  } deriving (Show, Eq, Generic)

buildName :: IO String
buildName = pure "Fancy Generated Name"

buildCompany :: Rec ("name" .== Maybe String) -> IO Company
buildCompany ps = do
  companyName <- fromParams (ps .! #name) buildName
  pure $ Company {..}

buildProduct ::
     Rec ("name" .== Maybe String .+ "companyId" .== Maybe CompanyId)
  -> IO Product
buildProduct ps = do
  productName <- fromParams (ps .! #name) buildName
  productCompanyId <- fromParamsIns (ps .! #companyId) (buildCompany (#name .== Nothing))
  pure $ Product {..}

-- | think persistent's `insert` here, returning record id
insertInDb :: a -> IO Int
insertInDb _ = pure 1

fromParams :: Applicative f => Maybe a -> f a -> f a
fromParams mv builder = do
  case mv of
    Nothing -> builder
    Just v -> pure v

fromParamsIns :: Maybe Int -> IO a -> IO Int
fromParamsIns mv builder = do
  case mv of
    Nothing -> do
      v <- builder
      insertInDb v
    Just v -> pure v

main :: IO ()
main = do
  putStrLn "hello world"
