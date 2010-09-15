{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bindings.LevMar
-- Copyright   :  (c) 2009-2010 Roel van Dijk & Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  vandijk.roel@gmail.com, v.dijk.bas@gmail.com
-- Stability   :  Experimental
--
-- A low level binding to the C levmar (Levenberg-Marquardt) library.
--
-- For documentation see: <http://www.ics.forth.gr/~lourakis/levmar/>
--
--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include <lm.h>

module Bindings.LevMar
    ( c'LM_VERSION

      -- * Maximum sizes of arrays.
    , c'LM_OPTS_SZ
    , c'LM_INFO_SZ

      -- * Errors.
    , c'LM_ERROR
    , c'LM_ERROR_LAPACK_ERROR
    , c'LM_ERROR_NO_JACOBIAN
    , c'LM_ERROR_NO_BOX_CONSTRAINTS
    , c'LM_ERROR_FAILED_BOX_CHECK
    , c'LM_ERROR_MEMORY_ALLOCATION_FAILURE
    , c'LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS
    , c'LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK
    , c'LM_ERROR_TOO_FEW_MEASUREMENTS
    , c'LM_ERROR_SINGULAR_MATRIX
    , c'LM_ERROR_SUM_OF_SQUARES_NOT_FINITE

      -- * Default values for minimization options.
    , c'LM_INIT_MU
    , c'LM_STOP_THRESH
    , c'LM_DIFF_DELTA

      -- * Handy type synonyms
    , Parameters
    , Measurements
    , Options
    , LowerBounds
    , UpperBounds
    , ConstraintsMatrix
    , ConstraintsVector
    , Weights
    , Info
    , Work
    , Covar
    , AData
    , NrOfParameters
    , NrOfMeasurements
    , NrOfConstraints
    , MaxIterations

      -- * Model & Jacobian.
    , Model
    , Jacobian

    , withModel
    , withJacobian

      -- * Types of the Levenberg-Marquardt algorithms.
    , LevMarDer
    , LevMarDif
    , LevMarBCDer
    , LevMarBCDif
    , LevMarLecDer
    , LevMarLecDif
    , LevMarBLecDer
    , LevMarBLecDif

      -- * Levenberg-Marquardt algorithms.
    , c'dlevmar_der
    , c'slevmar_der
    , c'dlevmar_dif
    , c'slevmar_dif
    , c'dlevmar_bc_der
    , c'slevmar_bc_der
    , c'dlevmar_bc_dif
    , c'slevmar_bc_dif
    , c'dlevmar_lec_der
    , c'slevmar_lec_der
    , c'dlevmar_lec_dif
    , c'slevmar_lec_dif
    , c'dlevmar_blec_der
    , c'slevmar_blec_der
    , c'dlevmar_blec_dif
    , c'slevmar_blec_dif

      -- * Jacobian verification
    , Errors
    , LevMarChkJac
    , c'dlevmar_chkjac
    , c'slevmar_chkjac

      -- * Utils
    , BestFitParameterIx

    , LevMarStddev
    , LevMarCorCoef
    , LevMarR2

    , Result

    , c'dlevmar_stddev
    , c'slevmar_stddev
    , c'dlevmar_corcoef
    , c'slevmar_corcoef
    , c'dlevmar_R2
    , c'slevmar_R2
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Prelude           ( Num, Fractional, Double, Float )

#if __GLASGOW_HASKELL__ < 700
import Prelude           ( negate, fromInteger, fromRational )
#endif

import Data.Int          ( Int )
import System.IO         ( IO )
import Prelude           ( String )
import Foreign.Ptr       ( Ptr, FunPtr, freeHaskellFunPtr )
import Control.Exception ( bracket )


--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | The version of the C levmar library.
c'LM_VERSION :: String
c'LM_VERSION = #const_str LM_VERSION


--------------------------------------------------------------------------------
-- Maximum sizes of arrays.
--------------------------------------------------------------------------------

-- | The maximum size of the options array.
#num LM_OPTS_SZ

-- | The size of the info array.
#num LM_INFO_SZ


--------------------------------------------------------------------------------
-- Errors.
--------------------------------------------------------------------------------

#num LM_ERROR
#num LM_ERROR_LAPACK_ERROR
#num LM_ERROR_NO_JACOBIAN
#num LM_ERROR_NO_BOX_CONSTRAINTS
#num LM_ERROR_FAILED_BOX_CHECK
#num LM_ERROR_MEMORY_ALLOCATION_FAILURE
#num LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS
#num LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK
#num LM_ERROR_TOO_FEW_MEASUREMENTS
#num LM_ERROR_SINGULAR_MATRIX
#num LM_ERROR_SUM_OF_SQUARES_NOT_FINITE


--------------------------------------------------------------------------------
-- Default values for minimization options.
--------------------------------------------------------------------------------

#fractional LM_INIT_MU
#fractional LM_STOP_THRESH
#fractional LM_DIFF_DELTA


--------------------------------------------------------------------------------
-- Handy type synonyms
--------------------------------------------------------------------------------

type Parameters        = Ptr
type Measurements      = Ptr
type Options           = Ptr
type LowerBounds       = Ptr
type UpperBounds       = Ptr
type ConstraintsMatrix = Ptr
type ConstraintsVector = Ptr
type Weights           = Ptr
type Info              = Ptr
type Work              = Ptr
type Covar             = Ptr
type AData             = Ptr ()
type NrOfParameters    = Int
type NrOfMeasurements  = Int
type NrOfConstraints   = Int
type MaxIterations     = Int


--------------------------------------------------------------------------------
-- Model & Jacobian.
--------------------------------------------------------------------------------

-- | Functional relation describing measurements.
type Model r =  Parameters r
             -> Measurements r
             -> NrOfParameters
             -> NrOfMeasurements
             -> AData
             -> IO ()

type Jacobian a = Model a

foreign import ccall "wrapper" mkModel :: Model a -> IO (FunPtr (Model a))

mkJacobian :: Jacobian a -> IO (FunPtr (Jacobian a))
mkJacobian = mkModel

withModel :: Model a -> (FunPtr (Model a) -> IO b) -> IO b
withModel m = bracket (mkModel m) freeHaskellFunPtr

withJacobian :: Jacobian a -> (FunPtr (Jacobian a) -> IO b) -> IO b
withJacobian j = bracket (mkJacobian j) freeHaskellFunPtr


--------------------------------------------------------------------------------
-- Types of the Levenberg-Marquardt algorithms.
--------------------------------------------------------------------------------

type LevMarDer r =  FunPtr (Model r)
                 -> FunPtr (Jacobian r)
                 -> Parameters r
                 -> Measurements r
                 -> NrOfParameters
                 -> NrOfMeasurements
                 -> MaxIterations
                 -> Options r
                 -> Info r
                 -> Work r
                 -> Covar r
                 -> AData
                 -> IO Int

type LevMarDif r =  FunPtr (Model r)
                 -> Parameters r
                 -> Measurements r
                 -> NrOfParameters
                 -> NrOfMeasurements
                 -> MaxIterations
                 -> Options r
                 -> Info r
                 -> Work r
                 -> Covar r
                 -> AData
                 -> IO Int

type LevMarBCDer r =  FunPtr (Model r)
                   -> FunPtr (Jacobian r)
                   -> Parameters r
                   -> Measurements r
                   -> NrOfParameters
                   -> NrOfMeasurements
                   -> LowerBounds r
                   -> UpperBounds r
                   -> MaxIterations
                   -> Options r
                   -> Info r
                   -> Work r
                   -> Covar r
                   -> AData
                   -> IO Int

type LevMarBCDif r =  FunPtr (Model r)
                   -> Parameters r
                   -> Measurements r
                   -> NrOfParameters
                   -> NrOfMeasurements
                   -> LowerBounds r
                   -> UpperBounds r
                   -> MaxIterations
                   -> Options r
                   -> Info r
                   -> Work r
                   -> Covar r
                   -> AData
                   -> IO Int

type LevMarLecDer r =  FunPtr (Model r)
                    -> FunPtr (Jacobian r)
                    -> Parameters r
                    -> Measurements r
                    -> NrOfParameters
                    -> NrOfMeasurements
                    -> ConstraintsMatrix r
                    -> ConstraintsVector r
                    -> NrOfConstraints
                    -> MaxIterations
                    -> Options r
                    -> Info r
                    -> Work r
                    -> Covar r
                    -> AData
                    -> IO Int

type LevMarLecDif r =  FunPtr (Model r)
                    -> Parameters r
                    -> Measurements r
                    -> NrOfParameters
                    -> NrOfMeasurements
                    -> ConstraintsMatrix r
                    -> ConstraintsVector r
                    -> NrOfConstraints
                    -> MaxIterations
                    -> Options r
                    -> Info r
                    -> Work r
                    -> Covar r
                    -> AData
                    -> IO Int

type LevMarBLecDer r =  FunPtr (Model r)
                     -> FunPtr (Jacobian r)
                     -> Parameters r
                     -> Measurements r
                     -> NrOfParameters
                     -> NrOfMeasurements
                     -> LowerBounds r
                     -> UpperBounds r
                     -> ConstraintsMatrix r
                     -> ConstraintsVector r
                     -> NrOfConstraints
                     -> Weights r
                     -> MaxIterations
                     -> Options r
                     -> Info r
                     -> Work r
                     -> Covar r
                     -> AData
                     -> IO Int

type LevMarBLecDif r =  FunPtr (Model r)
                     -> Parameters r
                     -> Measurements r
                     -> NrOfParameters
                     -> NrOfMeasurements
                     -> LowerBounds r
                     -> UpperBounds r
                     -> ConstraintsMatrix r
                     -> ConstraintsVector r
                     -> NrOfConstraints
                     -> Weights r
                     -> MaxIterations
                     -> Options r
                     -> Info r
                     -> Work r
                     -> Covar r
                     -> AData
                     -> IO Int


--------------------------------------------------------------------------------
-- Levenberg-Marquardt algorithms.
--------------------------------------------------------------------------------

#ccall slevmar_der      , LevMarDer     Float
#ccall dlevmar_der      , LevMarDer     Double
#ccall slevmar_dif      , LevMarDif     Float
#ccall dlevmar_dif      , LevMarDif     Double
#ccall slevmar_bc_der   , LevMarBCDer   Float
#ccall dlevmar_bc_der   , LevMarBCDer   Double
#ccall slevmar_bc_dif   , LevMarBCDif   Float
#ccall dlevmar_bc_dif   , LevMarBCDif   Double
#ccall slevmar_lec_der  , LevMarLecDer  Float
#ccall dlevmar_lec_der  , LevMarLecDer  Double
#ccall slevmar_lec_dif  , LevMarLecDif  Float
#ccall dlevmar_lec_dif  , LevMarLecDif  Double
#ccall slevmar_blec_der , LevMarBLecDer Float
#ccall dlevmar_blec_der , LevMarBLecDer Double
#ccall slevmar_blec_dif , LevMarBLecDif Float
#ccall dlevmar_blec_dif , LevMarBLecDif Double


--------------------------------------------------------------------------------
-- Jacobian verification
--------------------------------------------------------------------------------

type Errors = Ptr

type LevMarChkJac r =  FunPtr (Model r)
                    -> FunPtr (Jacobian r)
                    -> Parameters r
                    -> NrOfParameters
                    -> NrOfMeasurements
                    -> AData
                    -> Errors r
                    -> IO ()

#ccall dlevmar_chkjac , LevMarChkJac Double
#ccall slevmar_chkjac , LevMarChkJac Float


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

type BestFitParameterIx = Int

-- | Standard deviation.
type LevMarStddev r =  Covar r
                    -> NrOfParameters
                    -> BestFitParameterIx
                    -> IO r

-- | Pearson's correlation coefficient for best-fit parameters.
type LevMarCorCoef r =  Covar r
                     -> NrOfParameters
                     -> BestFitParameterIx
                     -> BestFitParameterIx
                     -> IO r

-- | Coefficient of determination (R2).
type LevMarR2 r =  FunPtr (Model r)
                -> Parameters r
                -> Measurements r
                -> NrOfParameters
                -> NrOfMeasurements
                -> AData
                -> Result r
                -> IO Int

type Result = Ptr

#ccall dlevmar_stddev  , LevMarStddev  Double
#ccall slevmar_stddev  , LevMarStddev  Float
#ccall dlevmar_corcoef , LevMarCorCoef Double
#ccall slevmar_corcoef , LevMarCorCoef Float
#ccall dlevmar_R2      , LevMarR2      Double
#ccall slevmar_R2      , LevMarR2      Float


-- The End ---------------------------------------------------------------------
