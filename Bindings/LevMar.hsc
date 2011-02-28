{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bindings.LevMar
-- Copyright   :  (c) 2009 Roel van Dijk & Bas van Dijk
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

import Prelude           ( Num, Fractional )

#if __GLASGOW_HASKELL__ < 700
import Prelude           ( negate, fromInteger, fromRational )
#endif

import System.IO         ( IO )
import Prelude           ( String )
import Foreign.C.Types   ( CInt, CFloat, CDouble )
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
type NrOfParameters    = CInt
type NrOfMeasurements  = CInt
type NrOfConstraints   = CInt
type MaxIterations     = CInt


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

type LevMarDer cr =  FunPtr (Model cr)
                  -> FunPtr (Jacobian cr)
                  -> Parameters cr
                  -> Measurements cr
                  -> NrOfParameters
                  -> NrOfMeasurements
                  -> MaxIterations
                  -> Options cr
                  -> Info cr
                  -> Work cr
                  -> Covar cr
                  -> AData
                  -> IO CInt

type LevMarDif cr =  FunPtr (Model cr)
                  -> Parameters cr
                  -> Measurements cr
                  -> NrOfParameters
                  -> NrOfMeasurements
                  -> MaxIterations
                  -> Options cr
                  -> Info cr
                  -> Work cr
                  -> Covar cr
                  -> AData
                  -> IO CInt

type LevMarBCDer cr =  FunPtr (Model cr)
                    -> FunPtr (Jacobian cr)
                    -> Parameters cr
                    -> Measurements cr
                    -> NrOfParameters
                    -> NrOfMeasurements
                    -> LowerBounds cr
                    -> UpperBounds cr
                    -> MaxIterations
                    -> Options cr
                    -> Info cr
                    -> Work cr
                    -> Covar cr
                    -> AData
                    -> IO CInt

type LevMarBCDif cr =  FunPtr (Model cr)
                    -> Parameters cr
                    -> Measurements cr
                    -> NrOfParameters
                    -> NrOfMeasurements
                    -> LowerBounds cr
                    -> UpperBounds cr
                    -> MaxIterations
                    -> Options cr
                    -> Info cr
                    -> Work cr
                    -> Covar cr
                    -> AData
                    -> IO CInt

type LevMarLecDer cr =  FunPtr (Model cr)
                     -> FunPtr (Jacobian cr)
                     -> Parameters cr
                     -> Measurements cr
                     -> NrOfParameters
                     -> NrOfMeasurements
                     -> ConstraintsMatrix cr
                     -> ConstraintsVector cr
                     -> NrOfConstraints
                     -> MaxIterations
                     -> Options cr
                     -> Info cr
                     -> Work cr
                     -> Covar cr
                     -> AData
                     -> IO CInt

type LevMarLecDif cr =  FunPtr (Model cr)
                     -> Parameters cr
                     -> Measurements cr
                     -> NrOfParameters
                     -> NrOfMeasurements
                     -> ConstraintsMatrix cr
                     -> ConstraintsVector cr
                     -> NrOfConstraints
                     -> MaxIterations
                     -> Options cr
                     -> Info cr
                     -> Work cr
                     -> Covar cr
                     -> AData
                     -> IO CInt

type LevMarBLecDer cr =  FunPtr (Model cr)
                      -> FunPtr (Jacobian cr)
                      -> Parameters cr
                      -> Measurements cr
                      -> NrOfParameters
                      -> NrOfMeasurements
                      -> LowerBounds cr
                      -> UpperBounds cr
                      -> ConstraintsMatrix cr
                      -> ConstraintsVector cr
                      -> NrOfConstraints
                      -> Weights cr
                      -> MaxIterations
                      -> Options cr
                      -> Info cr
                      -> Work cr
                      -> Covar cr
                      -> AData
                      -> IO CInt

type LevMarBLecDif cr =  FunPtr (Model cr)
                      -> Parameters cr
                      -> Measurements cr
                      -> NrOfParameters
                      -> NrOfMeasurements
                      -> LowerBounds cr
                      -> UpperBounds cr
                      -> ConstraintsMatrix cr
                      -> ConstraintsVector cr
                      -> NrOfConstraints
                      -> Weights cr
                      -> MaxIterations
                      -> Options cr
                      -> Info cr
                      -> Work cr
                      -> Covar cr
                      -> AData
                      -> IO CInt


--------------------------------------------------------------------------------
-- Levenberg-Marquardt algorithms.
--------------------------------------------------------------------------------

#ccall slevmar_der      , LevMarDer     CFloat
#ccall dlevmar_der      , LevMarDer     CDouble
#ccall slevmar_dif      , LevMarDif     CFloat
#ccall dlevmar_dif      , LevMarDif     CDouble
#ccall slevmar_bc_der   , LevMarBCDer   CFloat
#ccall dlevmar_bc_der   , LevMarBCDer   CDouble
#ccall slevmar_bc_dif   , LevMarBCDif   CFloat
#ccall dlevmar_bc_dif   , LevMarBCDif   CDouble
#ccall slevmar_lec_der  , LevMarLecDer  CFloat
#ccall dlevmar_lec_der  , LevMarLecDer  CDouble
#ccall slevmar_lec_dif  , LevMarLecDif  CFloat
#ccall dlevmar_lec_dif  , LevMarLecDif  CDouble
#ccall slevmar_blec_der , LevMarBLecDer CFloat
#ccall dlevmar_blec_der , LevMarBLecDer CDouble
#ccall slevmar_blec_dif , LevMarBLecDif CFloat
#ccall dlevmar_blec_dif , LevMarBLecDif CDouble


--------------------------------------------------------------------------------
-- Jacobian verification
--------------------------------------------------------------------------------

type Errors = Ptr

type LevMarChkJac cr =  FunPtr (Model cr)
                     -> FunPtr (Jacobian cr)
                     -> Parameters cr
                     -> NrOfParameters
                     -> NrOfMeasurements
                     -> AData
                     -> Errors cr
                     -> IO ()

#ccall dlevmar_chkjac , LevMarChkJac CDouble
#ccall slevmar_chkjac , LevMarChkJac CFloat


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

type BestFitParameterIx = CInt

-- | Standard deviation.
type LevMarStddev cr =  Covar cr
                     -> NrOfParameters
                     -> BestFitParameterIx
                     -> IO cr

-- | Pearson's correlation coefficient for best-fit parameters.
type LevMarCorCoef cr =  Covar cr
                      -> NrOfParameters
                      -> BestFitParameterIx
                      -> BestFitParameterIx
                      -> IO cr

-- | Coefficient of determination (R2).
type LevMarR2 cr =  FunPtr (Model cr)
                 -> Parameters cr
                 -> Measurements cr
                 -> NrOfParameters
                 -> NrOfMeasurements
                 -> AData
                 -> Result cr
                 -> IO CInt

type Result = Ptr

#ccall dlevmar_stddev  , LevMarStddev  CDouble
#ccall slevmar_stddev  , LevMarStddev  CFloat
#ccall dlevmar_corcoef , LevMarCorCoef CDouble
#ccall slevmar_corcoef , LevMarCorCoef CFloat
#ccall dlevmar_R2      , LevMarR2      CDouble
#ccall slevmar_R2      , LevMarR2      CFloat


-- The End ---------------------------------------------------------------------
