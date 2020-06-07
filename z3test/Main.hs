module Main where

import Control.Applicative
import Data.Maybe
import qualified Data.Traversable as T
import Control.Monad

import Z3.Monad
{-
script :: Z3 (Maybe [Integer])
script = do
  q1 <- mkFreshIntVar "q1"
  q2 <- mkFreshIntVar "q2"
  q3 <- mkFreshIntVar "q3"
  q4 <- mkFreshIntVar "q4"
  _1 <- mkInteger 1
  _4 <- mkInteger 4
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  assert =<< mkAnd =<< T.sequence
    [ mkLe _1 q1, mkLe q1 _4  -- 1 <= q1 <= 4
    , mkLe _1 q2, mkLe q2 _4
    , mkLe _1 q3, mkLe q3 _4
    , mkLe _1 q4, mkLe q4 _4
    ]
  -- different columns
  assert =<< mkDistinct [q1,q2,q3,q4]
  -- avoid diagonal attacks
  assert =<< mkNot =<< mkOr =<< T.sequence
    [ diagonal 1 q1 q2  -- diagonal line of attack between q1 and q2
    , diagonal 2 q1 q3
    , diagonal 3 q1 q4
    , diagonal 1 q2 q3
    , diagonal 2 q2 q4
    , diagonal 1 q3 q4
    ]
  -- check and get solution
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) [q1,q2,q3,q4]
  where mkAbs x = do
          _0 <- mkInteger 0
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInteger d)
-}

getSolution :: [AST] -> Z3 (Maybe [(String,Integer)])
getSolution asts = do
	(_,mb_model) <- getModel
	case mb_model of
		Nothing -> return Nothing
		Just m -> (return.Just) =<< ( forM asts $ \ ast -> do
			Just val <- evalInt m ast
			name <- astToString ast
			return (name,val) )

looplength :: Z3 (Maybe [(String,Integer)])
looplength = do
	_0 <- mkInteger 0
	_32 <- mkInteger 32
	n <- mkFreshIntVar "n"
--	assert =<< mkGe n =<< mkInteger 25
--	assert =<< mkGe _32 n

	bit0 <- mkFreshBvVar "bit0" 32
	_27 <- mkInteger 27
	assert =<< mkEq _27 =<< mkBv2int bit0 False

	shr <- mkBvlshr bit0 =<< mkInt2bv 32 n
	
	_1 <- mkInteger 1
	n_plus_1bv <- mkInt2bv 32 =<< mkAdd [n,_1]
	shr1 <- mkBvlshr bit0 n_plus_1bv
	
	ibit <- mkBv2int shr False
	ibit1 <- mkBv2int shr1 False
	assert =<< mkNot =<< mkEq ibit _0
	assert =<< mkEq ibit1 _0
	getSolution [n]

main :: IO ()
main = evalZ3With Nothing (opt "smt.relevancy" (0::Int)) looplength >>= print
