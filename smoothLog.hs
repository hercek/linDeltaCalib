import Numeric
import Data.Maybe
import Data.List
import System.IO
import System.Exit
import System.IO.Error
import System.Environment
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

stepsPerMm :: Double
stepsPerMm = 80

xInc :: Double
xInc = 4

yInc :: Double
yInc = xInc * sqrt(2*2-1*1)

xyStart :: Double
xyStart = 88

data TKey = Key
  {mXi :: !Int
  ,mYi :: !Int
  } deriving (Eq, Ord, Show)

data TAvgInfo = AvgInfo
  {mK :: !TKey
  ,mW :: !Double
  } deriving Show

data TProbePoint = ProbePoint
  {mX :: !Double -- ^ cartesian coordinate X
  ,mY :: !Double -- ^ cartesian coordinate Y
  ,mZ :: !Double -- ^ cartesian coordinate Z
  ,mA :: !Int -- ^ delta coordinate A
  ,mB :: !Int -- ^ delta coordinate B
  ,mC :: !Int -- ^ delta coordinate C
  } deriving Show


avgArea, avgArea1, avgArea2 :: [TKey]
avgArea1 =
  [Key   0    0
  ,Key   2    0
  ,Key   1    1
  ,Key (-1)   1
  ,Key (-2)   0
  ,Key (-1) (-1)
  ,Key   1  (-1) ]
avgArea2 =
  [Key   4    0
  ,Key   3    1
  ,Key   2    2
  ,Key   0    2
  ,Key (-2)   2
  ,Key (-3)   1
  ,Key (-4)   0
  ,Key (-3) (-1)
  ,Key (-2) (-2)
  ,Key (-0) (-2)
  ,Key   2  (-2)
  ,Key   3  (-1) ]
avgArea = avgArea1 ++ avgArea2


midWeight :: Double
midWeight = 2 / toEnum (length avgArea)


-- gausian filter
g, g' :: Double -> Double -> Double
g x c = midWeight * exp(-c*x)
g' x c = midWeight * exp(-c*x) * (-x)


sqr :: Int -> Double -> Double
sqr a inc =
  let ra = toEnum a * inc in
  ra*ra


fnOnArea :: (Double -> Double -> Double) -> Double -> Double
fnOnArea fn c =
  let f p (Key x y) = p + fn (sqr x xInc + sqr y yInc) c in
  foldl' f 0 avgArea


myC :: Double
myC = go $ - log 0.5 / (4.0*xInc*xInc)
 where
  go c =
    let gRv = fnOnArea g c - 1.0 in
    if abs gRv < 1e-9 then c else
    let gRv' = fnOnArea g' c in
    let newC = c - gRv / gRv' in
    go newC


avgDef :: [TAvgInfo]
avgDef =
  let getWeight (Key x y) = g (sqr x xInc + sqr y yInc) myC in
  map (\k -> AvgInfo k (getWeight k)) avgArea


toKey :: TProbePoint -> TKey
toKey pp = Key (round $ (mX pp + xyStart)/xInc) (round $ (mY pp + xyStart)/yInc)

addKeys :: TKey -> TKey -> TKey
addKeys (Key x1 y1) (Key x2 y2) = Key (x1+x2) (y1+y2)

addPoints :: TProbePoint -> TProbePoint -> TProbePoint
addPoints a b = ProbePoint (mX a + mX b) (mY a + mY b) (mZ a + mZ b) (mA a + mA b) (mB a + mB b) (mC a + mC b)

scalePoint :: Double -> TProbePoint -> TProbePoint
scalePoint s a =
  let scale fn = round $ toEnum (fn a) * s in
  ProbePoint (mX a * s) (mY a * s) (mZ a * s) (scale mA) (scale mB) (scale mC)

main :: IO ()
main = do
  logFileNames <- getArgs
  pointMapLst <- catchIOError (mapM readLogFile logFileNames) $ \e ->
    hPrint stderr e >> hPutStrLn stderr "Ussage: smoothLog <logFileName>*" >> exitWith (ExitFailure 1)
  if null logFileNames then mapM_ (hPrint stderr) avgDef else do
  let pointMap = averagePointMaps pointMapLst
  let filteredPoints = catMaybes $ map (filterProbePoint pointMap) $ Map.elems pointMap
  mapM_ printProbePoint filteredPoints


readLogFile :: String -> IO (Map.Map TKey TProbePoint)
readLogFile fileName = do
  hnd <- openFile fileName ReadMode
  logText <- hGetContents hnd
  let ps = readPoints Nothing $ lines logText
  return $ foldl' (\m p -> Map.insert (toKey p) p m) Map.empty ps


averagePointMaps :: [Map.Map TKey TProbePoint] -> Map.Map TKey TProbePoint
averagePointMaps [] = Map.empty
averagePointMaps pms@(pm:_) =
  let pss = map (\k -> catMaybes $ map (Map.lookup k) pms) $ Map.keys pm in
  foldl' addPointAvgToMap Map.empty pss
  where
    addPointAvgToMap m [] = m
    addPointAvgToMap m ps@(_:_) =
      let pointAverage = scalePoint (1.0 / (toEnum $length ps)) (foldl1' addPoints ps) in
      Map.insert (toKey pointAverage) pointAverage m


printProbePoint :: TProbePoint -> IO()
printProbePoint p = do
   putStrLn $ showString "Z-probe:" $ showFFloat (Just 3) (mZ p) $
     showString " X:" $ shows (mX p) $ showString " Y:" $ shows (mY p) ""
   putStrLn $ showString "Measure/delta (Steps) =" $
     shows (mA p) $ showChar ',' $ shows (mB p) $ showChar ',' $ shows (mC p) ""


filterProbePoint :: Map.Map TKey TProbePoint -> TProbePoint -> Maybe TProbePoint
filterProbePoint pm p =
  let avgData = map  (\ai -> AvgInfo (addKeys (mK ai) (toKey p)) (mW ai)) avgDef in
  let zVals = catMaybes $ map (fmap mZ . flip Map.lookup pm . mK) avgData in
  if length zVals /= length avgDef then Nothing else
  let avg = foldr (\(z,w) s -> z*w + s) 0 $ zip zVals (map mW avgDef) in
  let corr = round $ (mZ p - avg) * stepsPerMm in
  Just $ p{ mZ = avg, mA = mA p + corr, mB = mB p + corr, mC = mC p + corr }


readPoints :: Maybe (Double,Double,Double) -> [String] -> [TProbePoint]
readPoints _ [] = []
readPoints Nothing (l:ls) = readPoints (readCartPoint l) ls
readPoints mbCartPoint@(Just (x,y,z)) (l:ls) =
  case readDeltaPoint l of
    Nothing ->
      case readCartPoint l of
        Nothing -> readPoints mbCartPoint ls
        newCartPoint@(Just _) -> readPoints newCartPoint ls
    Just (a,b,c) -> (ProbePoint x y z a b c) : readPoints Nothing ls


readCartPoint :: String -> Maybe (Double, Double, Double)
readCartPoint s = fmap fst $ listToMaybe $ readP_to_S readsCartPoint s
 where
  readsCartPoint = do
    skipMany get
    _ <- string "Z-probe:"
    z <- readS_to_P reads
    skipSpaces
    _ <- string "X:"
    x <- readS_to_P reads
    skipSpaces
    _ <- string "Y:"
    y <- readS_to_P reads
    return (x,y,z)


readDeltaPoint :: String -> Maybe (Int, Int, Int)
readDeltaPoint s = fmap fst $ listToMaybe $ readP_to_S readsDeltaPoint s
 where
  readsDeltaPoint = do
    skipMany get
    _ <- string "Measure/delta (Steps) ="
    a <- readS_to_P reads
    _ <- char ','
    b <- readS_to_P reads
    _ <- char ','
    c <- readS_to_P reads
    return (a,b,c)

