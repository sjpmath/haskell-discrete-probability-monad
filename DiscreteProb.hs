data ProbDist a = Prob [(a,Double)]
	deriving (Eq, Show)

unwrap :: ProbDist a -> [(a,Double)]
unwrap (Prob l) = l

instance Functor ProbDist where
 	fmap f (Prob l) = Prob [(f x, p) | (x, p) <- l]

instance Applicative ProbDist where
	pure x = Prob [(x,1.0)]
	f <*> xm = Prob [(f x, px*pf) | (x,px) <- unwrap xm, (f,pf) <- unwrap f]

instance Monad ProbDist where
	return x = Prob [(x,1.0)]
	xm >>= f = Prob [(y, px*py) | (x,px) <- unwrap xm, (y, py) <- unwrap (f x)]

getProb :: Eq a => ProbDist a -> a -> Double
getProb (Prob xs) x = case xs of
	[] -> 0.0
	((y,p):ys) -> if x==y then (p + getProb (Prob ys) x) else getProb (Prob ys) x

observe :: Integer -> (Integer -> ProbDist Integer) -> ProbDist Integer -> ProbDist Integer
observe x model prior = Prob [(y, (getProb (model y) x) * p) | (y,p) <- unwrap prior]

unnormalizedPosterior :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> ProbDist Integer
unnormalizedPosterior prior model dat = foldr f prior dat where
	f d probdist = observe d model probdist

likelihoodmodel :: (Integer -> ProbDist Integer) -> [Integer] -> (Integer -> ProbDist [Integer])
likelihoodmodel model dat x = Prob [(dat, foldr (*) 1.0 (map (\d -> getProb (model x) d) dat))]

modelevidence :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> ProbDist [Integer]
modelevidence prior model dat = prior >>= (likelihoodmodel model dat)

normalizer :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> Double
normalizer prior model dat = getProb (modelevidence prior model dat) dat

normalizedPosterior :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> ProbDist Integer
normalizedPosterior prior model dat = Prob (map (\(x,p)->(x,p/norm)) (unwrap posterior)) where
	norm = normalizer prior model dat
	posterior = foldr f prior dat where
		f d probdist = observe d model probdist



--modelevidence :: ProbDist Integer -> (Integer -> ProbDist Integer) -> ProbDist Integer
--modelevidence prior model = prior >>= model

--normalizer :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> Double
--normalizer prior model dat = foldr (*) 1.0 (map (\d -> getProb (modelevidence prior model) d) dat)

--normalizedPosterior :: ProbDist Integer -> (Integer -> ProbDist Integer) -> [Integer] -> ProbDist Integer
--normalizedPosterior prior model dat = Prob (map (\(x,p)->(x,p/norm)) (unwrap posterior)) where
--	norm = foldr (*) 1.0 (map (\d -> getProb (modelevidence prior model) d) dat)
--	posterior = foldr f prior dat where
--		f d probdist = observe d model probdist




toy :: ProbDist Integer
toy = Prob [(0,0.25), (1,0.75)]

toy1 :: ProbDist Integer
toy1 = fmap ((*) 2) toy

toy2 :: ProbDist Integer
toy2 = toy >>= (\x -> Prob [(4,0.60), (5, 0.40)])

uniform :: Integer -> ProbDist Integer
uniform n = Prob [(i,(1.0)/(fromIntegral n)) | i <- [0..n-1]]



prior1 :: ProbDist Integer
prior1 = uniform 5

model1 :: Integer -> ProbDist Integer
model1 = \x -> uniform x

posterior1 :: ProbDist Integer
posterior1 = observe 2 model1 prior1

main :: IO ()
main = putStrLn ("Hello world")
