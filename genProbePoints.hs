import System.Random
import System.Time
import Numeric

xyRange :: Int
xyRange = 96-8

xHalfInc :: Int
xHalfInc = 4 -- 8, 6, or 4 for coarse, fine, or super fine

yInc :: Double
yInc = toEnum xHalfInc * sqrt(2*2-1*1)

radiusSq :: Double
radiusSq = 122.0*122.0

speed :: Int
speed = 90*60

height :: Int
height = 14

main :: IO ()
main = mkBgn >> getSeed >>= \s -> go (shuffle s points)
 where
  go [] = putStrLn "G1 X0 Y0 Z100"
  go ((x,y):rest) =
    if toEnum (x*x) + y*y > radiusSq then go rest else do
    putStrLn $ mkG0 x y
    putStrLn "G30" -- modify the probe command here!
    go rest

  mkBgn = mapM_ putStrLn
    [ "G28"
    , "G0 X0 Y0 Z" ++ show height
    , "G0 X-41.2 Y-23.8 F" ++ show speed
    , "G0 X0 Y-48"
    , "G0 X41.2 Y-23.8"
    , "G0 X41.2 Y23.8"
    , "G0 X0 Y48"
    , "G0 X-41.2 Y23.8"
    , "G0 X0 Y0" ]

  mkG0 x y =
    showString "G1 X" $ shows x $ showString " Y" $ showFFloat (Just 3) y $ showString " Z" $ show height

  getSeed =
    getClockTime >>= toCalendarTime >>= \t ->
    return $ ((ctDay t * 24 + ctHour t)*60 + ctMin t)*60 + ctSec t

points :: [(Int,Double)]
points =
  let columns  = [-xyRangeD, -xyRangeD+yInc .. xyRangeD] in
  concat $ makePairs True columns
  where
    makePairs _ [] = []
    makePairs True  (y:ys) = zip evenRows (repeat y) : makePairs False ys
    makePairs False (y:ys) = zip oddRows  (repeat y) : makePairs True  ys
    evenRows = [-xyRange,          -xyRange+2*xHalfInc .. xyRange]
    oddRows  = [-xyRange+xHalfInc, -xyRange+3*xHalfInc .. xyRange]
    xyRangeD = toEnum xyRange


shuffle :: Int -> [a] -> [a]
shuffle seed lst =
  let ll = length lst in
  if ll < 2 then lst else
  pick lst $ mkRandomSeq (ll-1) (randomRs (0, ll-1) $ mkStdGen seed) ll


pick :: [a] -> [Int] -> [a]
pick rv@[] _ = rv
pick rv@[_] _ = rv
pick _ [] = undefined
pick lst (p:ps) =
  let (prefix, x:sufix) = splitAt p lst in
  x : pick (prefix ++ sufix) ps


mkRandomSeq :: Int -> [Int] -> Int -> [Int]
mkRandomSeq maxRandomNumber randomNumbers rvLength = go randomNumbers rvLength
 where
  go [] _ = []
  go _ 0 = []
  go (r:rs) cnt =
    let upperLimit = cnt * ((maxRandomNumber+1) `div` cnt) in
    if r >= upperLimit then go rs cnt else
    (r `rem` cnt) : go rs (cnt-1)

