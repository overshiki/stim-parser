{-# LANGUAGE TypeFamilies #-}
module StimParser.Trans where 
import Control.Monad.State.Lazy
import Control.Monad

import StimParser.Expr
import Data.Maybe

type Env = State (Int, Coords)

-- questions raised:
  -- is that `Measure` the only case where we increase the count? 
    -- no, each Gpp also increases the count
  -- if `Measure` contains multiple qubits, do we increase the count by one for each qubit?
    -- yes
  -- how could we deal with the rec inside the Repeat block?
    -- just, expand it into List

-- TODO: SHIFT_COORDS will affect the coordinate in stim annotation

-- transform all (QRec Rec) into (Q Int)
class FlattenQ a where
  type Out a
  flattenQ :: a -> Env (Out a) 

instance FlattenQ Q where
  type Out Q = Q
  flattenQ (Q i) = return (Q i)
  flattenQ (QRec (Rec r)) = do 
    (count, _) <- get 
    return $ Q (count + r + 1)
  flattenQ _ = error "currently flattenQ does not support this qtype"

instance FlattenQ Gate where
  type Out Gate = Gate
  flattenQ (Gate gt qs) = do 
    nqs <- mapM flattenQ qs 
    return $ Gate gt nqs 

-- for each measure, we increase the count by 1
-- currently, we assume measure does not contains (QRec Rec)
instance FlattenQ Measure where
  type Out Measure = Measure
  flattenQ m@(Measure mty mph qs) = do 
    (count, cs) <- get 
    let ncount = count + length qs 
    put (ncount, cs) 
    return m

instance FlattenQ Gpp where
  type Out Gpp = Gpp
  flattenQ g = do 
    (count, cs) <- get 
    let ncount = count + 1
    put (ncount, cs) 
    return g

instance FlattenQ Noise where
  type Out Noise = Noise
  flattenQ (NoiseNormal nty tag phs qs) = do 
    nqs <- mapM flattenQ qs 
    return $ NoiseNormal nty tag phs nqs

  flattenQ n@(NoiseE {}) = return n

instance FlattenQ Ann where
  type Out Ann = Maybe Ann
  flattenQ (Ann SHIFT_COORDS fs qs) = do
    -- nqs <- mapM flattenQ qs
    (count, Coords cs) <- get
    let ncs = zipWith (+) fs cs
    put (count, Coords ncs)
    return Nothing

  flattenQ (Ann DETECTOR fs qs) = do
    (count, Coords cs) <- get
    let nfs = zipWith (+) fs cs
    nqs <- mapM flattenQ qs
    return $ Just $ Ann DETECTOR nfs nqs
    
  flattenQ (Ann aty fs qs) = do 
    nqs <- mapM flattenQ qs 
    return $ Just $ Ann aty fs nqs

instance FlattenQ Stim where
  type Out Stim = Maybe Stim
  flattenQ (StimG g) = (Just . StimG) <$> flattenQ g 
  flattenQ (StimM g) = (Just . StimM) <$> flattenQ g  
  flattenQ (StimGpp g) = (Just . StimGpp) <$> flattenQ g 
  flattenQ (StimNoise g) = (Just . StimNoise) <$> flattenQ g 
  flattenQ (StimAnn g) = do
    ng <- flattenQ g
    case ng of
      Nothing -> return Nothing
      Just nng -> return $ Just $ StimAnn nng
  flattenQ (StimList gs) = do
    ngs <- mapM flattenQ gs
    let nngs = map fromJust $ filter isJust ngs
    return $ Just $ StimList nngs
    -- StimList <$> mapM flattenQ gs
  flattenQ (StimRepeat n s) = do
    ns <- replicateM n (flattenQ s)
    let nns = map fromJust $ filter isJust ns
    return $ Just $ StimList nns
    -- StimList <$> replicateM n (flattenQ s)
