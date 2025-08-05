module StimParser.Trans where 
import Control.Monad.State.Lazy
import Control.Monad

import StimParser.Expr

type Env = State Int

-- questions raised:
  -- is that `Measure` the only case where we increase the count? 
    -- currently, the only case
  -- if `Measure` contains multiple qubits, do we increase the count by one for each qubit?
    -- currently, increase for each qubit
  -- how could we deal with the rec inside the Repeat block?
    -- currently, expand it into List

-- transform all (QRec Rec) into (Q Int)
class FlattenQ a where 
  flattenQ :: a -> Env a 

instance FlattenQ Q where 
  flattenQ (Q i) = return (Q i)
  flattenQ (QRec (Rec r)) = do 
    count <- get 
    return $ Q (count + r + 1)
  flattenQ _ = error "currently flattenQ does not support this qtype"

instance FlattenQ Gate where 
  flattenQ (Gate gt qs) = do 
    nqs <- mapM flattenQ qs 
    return $ Gate gt nqs 

-- for each measure, we increase the count by 1
-- currently, we assume measure does not contains (QRec Rec)
instance FlattenQ Measure where  
  flattenQ m@(Measure mty mph qs) = do 
    count <- get 
    let ncount = count + length qs 
    put ncount 
    return m

instance FlattenQ Gpp where 
  flattenQ g = return g

instance FlattenQ Noise where 
  flattenQ (NoiseNormal nty tag phs qs) = do 
    nqs <- mapM flattenQ qs 
    return $ NoiseNormal nty tag phs nqs

  flattenQ n@(NoiseE {}) = return n

instance FlattenQ Ann where 
  flattenQ (Ann aty fs qs) = do 
    nqs <- mapM flattenQ qs 
    return $ Ann aty fs nqs

instance FlattenQ Stim where 
  flattenQ (StimG g) = StimG <$> flattenQ g 
  flattenQ (StimM g) = StimM <$> flattenQ g  
  flattenQ (StimGpp g) = StimGpp <$> flattenQ g 
  flattenQ (StimNoise g) = StimNoise <$> flattenQ g 
  flattenQ (StimAnn g) = StimAnn <$> flattenQ g 
  flattenQ (StimList gs) = StimList <$> mapM flattenQ gs
  flattenQ (StimRepeat n s) = StimList <$> replicateM n (flattenQ s)
