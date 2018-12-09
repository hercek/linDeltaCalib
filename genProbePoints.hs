import System.Random
import System.Time

xyRange :: Int
xyRange = 96

xyInc :: Int
xyInc = 8 -- 16, 12, or 8 for coarse, fine, or super fine

radiusSq :: Int
radiusSq = 130*130

speed :: Int
speed = 90*60

height :: Int
height = 14

main :: IO ()
main = mkBgn >> getSeed >>= \s -> go (shuffle s points)
 where
  go [] = putStrLn "G1 X0 Y0 Z100"
  go ((x,y):rest) =
    if x*x + y*y > radiusSq then go rest else do
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
    showString "G1 X" $ shows x $ showString " Y" $ shows y $ showString " Z" $ show height

  getSeed =
    getClockTime >>= toCalendarTime >>= \t ->
    return $ ((ctDay t * 24 + ctHour t)*60 + ctMin t)*60 + ctSec t

points :: [(Int,Int)]
points =
  let range = [0-xyRange, xyInc-xyRange .. xyRange] in
  concat $ map (\r -> zip range (repeat r)) range


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

