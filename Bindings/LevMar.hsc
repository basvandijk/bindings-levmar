{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bindings.LevMar
-- Copyright   :  (c) 2009 Roel van Dijk & Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  vandijk.roel@gmail.com, v.dijk.bas@gmail.com
-- Stability   :  Experimental
--
-- A binding to the C levmar (Levenberg-Marquardt) library
--
-- For documentation see: <http://www.ics.forth.gr/~lourakis/levmar/>
--
--------------------------------------------------------------------------------

module Bindings.LevMar
    ( _LM_VERSION

      -- * Maximum sizes of arrays.
    , _LM_OPTS_SZ
    , _LM_INFO_SZ

      -- * Errors.
    , _LM_ERROR_LAPACK_ERROR
    , _LM_ERROR_NO_JACOBIAN
    , _LM_ERROR_NO_BOX_CONSTRAINTS
    , _LM_ERROR_FAILED_BOX_CHECK
    , _LM_ERROR_MEMORY_ALLOCATION_FAILURE
    , _LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS
    , _LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK
    , _LM_ERROR_TOO_FEW_MEASUREMENTS
    , _LM_ERROR_SINGULAR_MATRIX
    , _LM_ERROR_SUM_OF_SQUARES_NOT_FINITE

      -- * Default values for minimization options.
    , _LM_INIT_MU
    , _LM_STOP_THRESH
    , _LM_DIFF_DELTA

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
    , dlevmar_der
    , slevmar_der
    , dlevmar_dif
    , slevmar_dif
    , dlevmar_bc_der
    , slevmar_bc_der
    , dlevmar_bc_dif
    , slevmar_bc_dif
    , dlevmar_lec_der
    , slevmar_lec_der
    , dlevmar_lec_dif
    , slevmar_lec_dif
    , dlevmar_blec_der
    , slevmar_blec_der
    , dlevmar_blec_dif
    , slevmar_blec_dif

      -- * Jacobian verification
    , Errors
    , LevMarChkJac
    , dlevmar_chkjac
    , slevmar_chkjac

      -- * Utils
    , BestFitParameterIx

    , LevMarStddev
    , LevMarCorCoef
    , LevMarR2

    , Result

    , dlevmar_stddev
    , slevmar_stddev
    , dlevmar_corcoef
    , slevmar_corcoef
    , dlevmar_R2
    , slevmar_R2
    ) where


import Foreign.C.Types   ( CInt, CFloat, CDouble )
import Foreign.Ptr       ( Ptr, FunPtr, freeHaskellFunPtr )
import Control.Exception ( bracket )

#include <lm.h>


-- | The version of the C levmar library.
_LM_VERSION :: String
_LM_VERSION = #const_str LM_VERSION


--------------------------------------------------------------------------------
-- Maximum sizes of arrays.
--------------------------------------------------------------------------------

-- | The maximum size of the options array.
_LM_OPTS_SZ :: Int
_LM_OPTS_SZ = #const LM_OPTS_SZ

-- | The size of the info array.
_LM_INFO_SZ :: Int
_LM_INFO_SZ = #const LM_INFO_SZ


--------------------------------------------------------------------------------
-- Errors.
--------------------------------------------------------------------------------

#{enum CInt,
 , _LM_ERROR_LAPACK_ERROR              	          = LM_ERROR_LAPACK_ERROR
 , _LM_ERROR_NO_JACOBIAN               	          = LM_ERROR_NO_JACOBIAN
 , _LM_ERROR_NO_BOX_CONSTRAINTS        	          = LM_ERROR_NO_BOX_CONSTRAINTS
 , _LM_ERROR_FAILED_BOX_CHECK          	          = LM_ERROR_FAILED_BOX_CHECK
 , _LM_ERROR_MEMORY_ALLOCATION_FAILURE 	          = LM_ERROR_MEMORY_ALLOCATION_FAILURE
 , _LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS       = LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS
 , _LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK  = LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK
 , _LM_ERROR_TOO_FEW_MEASUREMENTS                 = LM_ERROR_TOO_FEW_MEASUREMENTS
 , _LM_ERROR_SINGULAR_MATRIX                      = LM_ERROR_SINGULAR_MATRIX
 , _LM_ERROR_SUM_OF_SQUARES_NOT_FINITE            = LM_ERROR_SUM_OF_SQUARES_NOT_FINITE
 }


--------------------------------------------------------------------------------
-- Default values for minimization options.
--------------------------------------------------------------------------------

#let const_real r = "%e", r

_LM_INIT_MU, _LM_STOP_THRESH, _LM_DIFF_DELTA :: Fractional a => a

_LM_INIT_MU     = #const_real LM_INIT_MU
_LM_STOP_THRESH = #const_real LM_STOP_THRESH
_LM_DIFF_DELTA  = #const_real LM_DIFF_DELTA


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

foreign import ccall "slevmar_der"      slevmar_der      :: LevMarDer     CFloat
foreign import ccall "dlevmar_der"      dlevmar_der      :: LevMarDer     CDouble
foreign import ccall "slevmar_dif"      slevmar_dif      :: LevMarDif     CFloat
foreign import ccall "dlevmar_dif"      dlevmar_dif      :: LevMarDif     CDouble
foreign import ccall "slevmar_bc_der"   slevmar_bc_der   :: LevMarBCDer   CFloat
foreign import ccall "dlevmar_bc_der"   dlevmar_bc_der   :: LevMarBCDer   CDouble
foreign import ccall "slevmar_bc_dif"   slevmar_bc_dif   :: LevMarBCDif   CFloat
foreign import ccall "dlevmar_bc_dif"   dlevmar_bc_dif   :: LevMarBCDif   CDouble
foreign import ccall "slevmar_lec_der"  slevmar_lec_der  :: LevMarLecDer  CFloat
foreign import ccall "dlevmar_lec_der"  dlevmar_lec_der  :: LevMarLecDer  CDouble
foreign import ccall "slevmar_lec_dif"  slevmar_lec_dif  :: LevMarLecDif  CFloat
foreign import ccall "dlevmar_lec_dif"  dlevmar_lec_dif  :: LevMarLecDif  CDouble
foreign import ccall "slevmar_blec_der" slevmar_blec_der :: LevMarBLecDer CFloat
foreign import ccall "dlevmar_blec_der" dlevmar_blec_der :: LevMarBLecDer CDouble
foreign import ccall "slevmar_blec_dif" slevmar_blec_dif :: LevMarBLecDif CFloat
foreign import ccall "dlevmar_blec_dif" dlevmar_blec_dif :: LevMarBLecDif CDouble


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

foreign import ccall "dlevmar_chkjac" dlevmar_chkjac :: LevMarChkJac CDouble
foreign import ccall "slevmar_chkjac" slevmar_chkjac :: LevMarChkJac CFloat


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

foreign import ccall "dlevmar_stddev"  dlevmar_stddev  :: LevMarStddev  CDouble
foreign import ccall "slevmar_stddev"  slevmar_stddev  :: LevMarStddev  CFloat
foreign import ccall "dlevmar_corcoef" dlevmar_corcoef :: LevMarCorCoef CDouble
foreign import ccall "slevmar_corcoef" slevmar_corcoef :: LevMarCorCoef CFloat
foreign import ccall "dlevmar_R2"      dlevmar_R2      :: LevMarR2      CDouble
foreign import ccall "slevmar_R2"      slevmar_R2      :: LevMarR2      CFloat


-- The End ---------------------------------------------------------------------
