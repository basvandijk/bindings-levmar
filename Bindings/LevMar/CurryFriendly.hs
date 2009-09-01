module Bindings.LevMar.CurryFriendly
    ( LMA_C._LM_OPTS_SZ
    , LMA_C._LM_INFO_SZ

    , LMA_C._LM_ERROR_LAPACK_ERROR
    , LMA_C._LM_ERROR_NO_JACOBIAN
    , LMA_C._LM_ERROR_NO_BOX_CONSTRAINTS
    , LMA_C._LM_ERROR_FAILED_BOX_CHECK
    , LMA_C._LM_ERROR_MEMORY_ALLOCATION_FAILURE
    , LMA_C._LM_ERROR_CONSTRAINT_MATRIX_ROWS_GT_COLS
    , LMA_C._LM_ERROR_CONSTRAINT_MATRIX_NOT_FULL_ROW_RANK
    , LMA_C._LM_ERROR_TOO_FEW_MEASUREMENTS
    , LMA_C._LM_ERROR_SINGULAR_MATRIX
    , LMA_C._LM_ERROR_SUM_OF_SQUARES_NOT_FINITE

    , LMA_C._LM_INIT_MU
    , LMA_C._LM_STOP_THRESH
    , LMA_C._LM_DIFF_DELTA

    , LMA_C._LM_VERSION

    , LMA_C.Model
    , LMA_C.Jacobian

    , LMA_C.withModel
    , LMA_C.withJacobian

    , LevMarDer
    , LevMarDif
    , LevMarBCDer
    , LevMarBCDif
    , LevMarLecDer
    , LevMarLecDif
    , LevMarBLecDer
    , LevMarBLecDif

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
    ) where

import Foreign.C.Types (CInt, CFloat, CDouble)
import Foreign.Ptr     (Ptr, FunPtr)

import qualified Bindings.LevMar as LMA_C

type BoxConstraints    cr a =  Ptr cr -- |Lower bounds
                            -> Ptr cr -- |Upper bounds
                            -> a

type LinearConstraints cr a =  Ptr cr -- |Constraints matrix
                            -> Ptr cr -- |Right hand constraints vector
                            -> CInt   -- |Number of constraints
                            -> a

type LevMarDif     cr = LMA_C.LevMarDif cr
type LevMarDer     cr = FunPtr (LMA_C.Jacobian cr) -> LevMarDif cr
type LevMarBCDif   cr = BoxConstraints cr (LevMarDif cr)
type LevMarBCDer   cr = BoxConstraints cr (LevMarDer cr)
type LevMarLecDif  cr = LinearConstraints cr (LevMarDif cr)
type LevMarLecDer  cr = LinearConstraints cr (LevMarDer cr)
type LevMarBLecDif cr = BoxConstraints cr (LinearConstraints cr (Ptr cr -> LevMarDif cr))
type LevMarBLecDer cr = BoxConstraints cr (LinearConstraints cr (Ptr cr -> LevMarDer cr))


mk_levmar_der :: LMA_C.LevMarDer cr -> LevMarDer cr
mk_levmar_der lma j f
            = lma f j

mk_levmar_bc_dif :: LMA_C.LevMarBCDif cr -> LevMarBCDif cr
mk_levmar_bc_dif lma lb ub f p x m n
               = lma f p x m n lb ub

mk_levmar_bc_der :: LMA_C.LevMarBCDer cr -> LevMarBCDer cr
mk_levmar_bc_der lma lb ub j f p x m n
               = lma f j p x m n lb ub

mk_levmar_lec_dif :: LMA_C.LevMarLecDif cr -> LevMarLecDif cr
mk_levmar_lec_dif lma a b k f p x m n
                = lma f p x m n a b k

mk_levmar_lec_der :: LMA_C.LevMarLecDer cr -> LevMarLecDer cr
mk_levmar_lec_der lma a b k j f p x m n
                = lma f j p x m n a b k

mk_levmar_blec_dif :: LMA_C.LevMarBLecDif cr -> LevMarBLecDif cr
mk_levmar_blec_dif lma lb ub a b k wghts f p x m n
                 = lma f p x m n lb ub a b k wghts

mk_levmar_blec_der :: LMA_C.LevMarBLecDer cr -> LevMarBLecDer cr
mk_levmar_blec_der lma lb ub a b k wghts j f p x m n
                 = lma f j p x m n lb ub a b k wghts


slevmar_dif :: LevMarDif CFloat
slevmar_dif = LMA_C.slevmar_dif

dlevmar_dif :: LevMarDif CDouble
dlevmar_dif = LMA_C.dlevmar_dif

slevmar_der :: LevMarDer CFloat
slevmar_der = mk_levmar_der LMA_C.slevmar_der

dlevmar_der :: LevMarDer CDouble
dlevmar_der = mk_levmar_der LMA_C.dlevmar_der

slevmar_bc_dif :: LevMarBCDif CFloat
slevmar_bc_dif = mk_levmar_bc_dif LMA_C.slevmar_bc_dif

dlevmar_bc_dif :: LevMarBCDif CDouble
dlevmar_bc_dif = mk_levmar_bc_dif LMA_C.dlevmar_bc_dif

slevmar_bc_der :: LevMarBCDer CFloat
slevmar_bc_der = mk_levmar_bc_der LMA_C.slevmar_bc_der

dlevmar_bc_der :: LevMarBCDer CDouble
dlevmar_bc_der = mk_levmar_bc_der LMA_C.dlevmar_bc_der

slevmar_lec_dif :: LevMarLecDif CFloat
slevmar_lec_dif = mk_levmar_lec_dif LMA_C.slevmar_lec_dif

dlevmar_lec_dif :: LevMarLecDif CDouble
dlevmar_lec_dif = mk_levmar_lec_dif LMA_C.dlevmar_lec_dif

slevmar_lec_der :: LevMarLecDer CFloat
slevmar_lec_der = mk_levmar_lec_der LMA_C.slevmar_lec_der

dlevmar_lec_der :: LevMarLecDer CDouble
dlevmar_lec_der = mk_levmar_lec_der LMA_C.dlevmar_lec_der

slevmar_blec_dif :: LevMarBLecDif CFloat
slevmar_blec_dif = mk_levmar_blec_dif LMA_C.slevmar_blec_dif

dlevmar_blec_dif :: LevMarBLecDif CDouble
dlevmar_blec_dif = mk_levmar_blec_dif LMA_C.dlevmar_blec_dif

slevmar_blec_der :: LevMarBLecDer CFloat
slevmar_blec_der = mk_levmar_blec_der LMA_C.slevmar_blec_der

dlevmar_blec_der :: LevMarBLecDer CDouble
dlevmar_blec_der = mk_levmar_blec_der LMA_C.dlevmar_blec_der
