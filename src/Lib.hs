module Lib
  ( printCheapestFourColoringOfUS
  ) where

-- This program calculates the least expensive coloring of a map of the
-- continental United States given that the four colors are worth 1, 2, 3, and 4
-- units each.
-- It uses this type of algorithm:
-- https://en.wikipedia.org/wiki/Branch_and_bound

-- FIXME wrap to 80 columns
-- FIXME more documentation?

import qualified Data.HashMap.Strict as M
import Data.List
import Data.List.Split

data State = WA | OR | ID | CA | NV | AZ | UT | MT | NM | WY | CO | ND | SD | NE | TX | KS | OK | MN | IA | MO | AR | LA | WI | IL | MS | IN | MI | TN | AL | KY | OH | GA | WV | SC | PA | FL | NC | VA | MD | NY | DC | VT | DE | NJ | CT | MA | RI | NH | ME deriving (Eq, Ord, Show)
type Color = Integer
type Coloring = (State, Color)
type Solution = [Coloring]

statesByLongitude = [WA, OR, ID, CA, NV, AZ, UT, MT, NM, WY, CO, ND, SD, NE, TX, KS, OK, MN, IA, MO, AR, LA, WI, IL, MS, IN, MI, TN, AL, KY, OH, GA, WV, SC, PA, FL, NC, VA, MD, NY, DC, VT, DE, NJ, CT, MA, RI, NH, ME]

-- Could have used an array here
adjacentStates :: State -> [State]
adjacentStates VT = [NY, MA, NH]
adjacentStates IL = [WI, IA, MO, KY, IN]
adjacentStates KS = [NE, CO, OK, MO]
adjacentStates NE = [SD, WY, CO, KS, MO, IA]
adjacentStates SC = [NC, GA]
adjacentStates NH = [VT, MA, ME]
adjacentStates IA = [MN, SD, NE, MO, IL, WI]
adjacentStates WY = [MT, ID, UT, CO, NE, SD]
adjacentStates MA = [NH, VT, NY, CT, RI]
adjacentStates OR = [WA, CA, ID, NV]
adjacentStates IN = [IL, KY, MI, OH]
adjacentStates TN = [KY, MO, AR, MS, AL, GA, NC, VA]
adjacentStates AZ = [UT, NV, CA, NM]
adjacentStates ID = [WA, OR, NV, UT, WY, MT]
adjacentStates NC = [VA, TN, GA, SC]
adjacentStates CO = [WY, UT, NM, OK, KS, NE]
adjacentStates DC = [VA, MD]
adjacentStates ME = [NH]
adjacentStates ND = [MT, SD, MN]
adjacentStates AL = [TN, MS, FL, GA]
adjacentStates DE = [PA, MD, NJ]
adjacentStates WV = [PA, OH, KY, VA, MD]
adjacentStates MO = [IA, NE, KS, OK, AR, TN, KY, IL]
adjacentStates NM = [CO, AZ, TX, OK]
adjacentStates NJ = [NY, PA, DE]
adjacentStates SD = [ND, MT, WY, NE, IA, MN]
adjacentStates MD = [DC, PA, WV, VA, DE]
adjacentStates VA = [DC, MD, WV, KY, TN, NC]
adjacentStates RI = [MA, CT]
adjacentStates CT = [MA, NY, RI]
adjacentStates UT = [ID, NV, AZ, CO, WY]
adjacentStates PA = [NY, OH, MD, DE, NJ, WV]
adjacentStates MT = [ID, WY, SD, ND]
adjacentStates OK = [KS, NM, TX, AR, MO, CO]
adjacentStates KY = [OH, IN, MO, TN, VA, WV, IL]
adjacentStates LA = [AR, TX, MS]
adjacentStates GA = [NC, TN, AL, FL, SC]
adjacentStates OH = [MI, IN, KY, WV, PA]
adjacentStates MS = [TN, AR, LA, AL]
adjacentStates WI = [MN, IA, IL, MI]
adjacentStates WA = [OR, ID]
adjacentStates MI = [WI, IN, OH]
adjacentStates TX = [OK, NM, LA, AR]
adjacentStates FL = [AL, GA]
adjacentStates CA = [OR, NV, AZ]
adjacentStates AR = [MO, OK, TX, LA, MS, TN]
adjacentStates MN = [ND, SD, IA, WI]
adjacentStates NY = [PA, NJ, CT, MA, VT]
adjacentStates NV = [OR, CA, AZ, UT, ID]

-- Could have used an array here
costOfState :: State -> Float
costOfState VT = 9249.56
costOfState NE = 76872.41
costOfState KS = 81814.88
costOfState IL = 55583.58
costOfState SC = 30109.47
costOfState NH = 8968.10
costOfState IA = 55869.36
costOfState WY = 97100.40
costOfState OR = 95996.79
costOfState MA = 7840.02
costOfState TN = 41217.12
costOfState IN = 35866.90
costOfState AZ = 113634.57
costOfState ID = 82747.21
costOfState NC = 48710.88
costOfState CO = 103717.53
costOfState DC = 61.40
costOfState ND = 68975.93
costOfState ME = 30861.55
costOfState AL = 50744.00
costOfState DE = 1953.56
costOfState WV = 24077.73
costOfState NM = 121355.53
costOfState MO = 68885.93
costOfState NJ = 7417.34
costOfState SD = 75884.64
costOfState MD = 9773.82
costOfState VA = 39594.07
costOfState RI = 1044.93
costOfState UT = 82143.65
costOfState CT = 4844.80
costOfState PA = 44816.61
costOfState OK = 68667.06
costOfState KY = 39728.18
costOfState MT = 145552.43
costOfState GA = 57906.14
costOfState LA = 43561.85
costOfState OH = 40948.38
costOfState MS = 46906.96
costOfState WI = 54310.10
costOfState WA = 66544.06
costOfState MI = 56803.82
costOfState TX = 261797.12
costOfState FL = 53926.82
costOfState CA = 155959.34
costOfState AR = 52068.17
costOfState MN = 79610.08
costOfState NV = 109825.99
costOfState NY = 47213.79

forbiddenColorings :: Solution -> State -> [Color]
forbiddenColorings [] state = []
forbiddenColorings ((stateOne, color):xs) stateTwo
    | elem stateOne (adjacentStates stateTwo) = Data.List.union [color] (forbiddenColorings xs stateTwo)
    | otherwise                               = forbiddenColorings xs stateTwo

allowedColorings :: Solution -> State -> [Color]
allowedColorings solution state = [1..4] \\ forbiddenColorings solution state

-- branch takes a list of solutions and a new state to consider and returns
-- a longer or equal list of solutions incorporating the new state. The
-- solutions are valid colorings but not necessarily optimal by cost
branch :: [Solution] -> State -> [Solution]
branch [] state = [[(state, 1)], [(state, 2)], [(state, 3)], [(state, 4)]]
branch solutionList state = branch' solutionList state
branch' [] state = []
branch' (x:xs) state = [[(state, c)] ++ x | c <- allowedColorings x state] ++ branch' xs state

-- I'm guessing this exists somewhere? Maybe without requiring things be Ord?
isSubsetOf :: Ord a => [a] -> [a] -> Bool
isSubsetOf a b = isSubsequenceOf (sort a) (sort b)

-- Given a list of states, return those that are bordered by a state not in the
-- list.
-- For example, [ME, NH] -> [NH] because ME only touches NH but NH touches others
interfaceStates :: [State] -> [State]
interfaceStates states = [ state | state <- states, not $ isSubsetOf (adjacentStates state) states]

-- Returns the list of solutions, removing those that are superfluous. For
-- example, if you passed [[(ME, 1), (NH, 2)], [(ME, 4), (NH, 2)]] it would
-- only return [[(ME, 1), (NH, 2)]] because the other map coloring is more
-- expensive but redundant because the coloring of ME doesn't matter to the
-- states we haven't considered yet.
-- Makes the assumption that all the listed solutions consider exactly the same
-- states.
bound :: [Solution] -> [Solution]
bound [] = []
bound solutions = [ head $ sortOn costOfSolution sol | sol <- grouped ]
    where
        grouped = groupBy (\x y -> (coloringsOfStates x ifaceStates) == (coloringsOfStates y ifaceStates)) sortedSolutions
        sortedSolutions = sortOn (\x -> coloringsOfStates x ifaceStates) solutions
        ifaceStates = interfaceStates $ map fst $ head solutions

-- Returns a list of the colors of the states given in a solution
-- FIXME is there a way to write this program without having Maybe?
coloringsOfStates :: Solution -> [State] -> [Maybe Color]
coloringsOfStates solution states = [ lookup state solution | state <- states ]

costOfSolution :: Solution -> Float
costOfSolution sol = sum $ map (\(state, color) -> fromIntegral color * costOfState state) sol

bestSolutions :: [State] -> [Solution]
bestSolutions [] = []
bestSolutions (state:xs) = bound $ branch (bestSolutions xs) state

printCheapestFourColoringOfUS :: IO ()
--printCheapestFourColoringOfUS = putStrLn $ show $ costOfSolution $ head $ bestSolutions $ statesByLongitude
printCheapestFourColoringOfUS = putStrLn $ formatArt asciiArt (head $ bestSolutions $ statesByLongitude)

formatArt :: String -> Solution -> String
formatArt string [] = string
formatArt string (x:xs) = replace (show (fst x)) (" " ++ show (snd x)) (formatArt string xs)

replace old new = intercalate new . splitOn old
asciiArt = unlines [
 "     ,__                                                  _,   ",
 "  \\~\\|  ~~---___              ,                          | \\   ",
 "   | WA   | |   ~~~~~~~|~~~~~| ~~---,   _            VT_/,ME>  ",
 "  /~-_--__| |  MT      |  ND \\  MN / ~\\~_/MI        /~| ||,'   ",
 "  | OR    |  \\         |------|   { WI / /~)     __-NY',|_\\,NH ",
 " /       |ID  |~~~~~~~~|  SD  \\    \\   | | '~\\  |_____,|~,-'MA ",
 " |~~--__ |    | WY     |____  |~~~~~|--| |__ /_-' PA  {,~CT RI ",
 " |   |  ~~~|~~|        |    ~~\\  IA /  `-' |`~ |~_____{/NJ     ",
 " |   | NV  |  '---------,  NE  \\----|   |IN|OH,' ~/~\\,|`MD DE  ",
 " ',  \\     | UT | CO    |~~~~~~~|    \\IL| ,'~~\\WV/ VA | DC     ",
 "  |CA \\    |    |       |   KS  | MO  \\_-~ KY /`~___--\\        ",
 "  ',   \\  ,-----|-------+-------'_____/__----~~/  NC  /        ",
 "   '_   '\\|     |      |~~~| OK  |    |  TN  _/-,~~-,/         ",
 "     \\    | AZ  | NM   |   |_    | AR /~~|~~\\    \\,/SC         ",
 "      ~~~-'     |      |     `~~~\\___|MS |AL | GA /            ",
 "          '-,_  | _____|          |  /   | ,-'---~\\            ",
 "              `~'~  \\      TX     |LA`--,~~~~-~~,FL\\           ",
 "                     \\/~\\      /~~~`---`         |  \\          ",
 "                         \\    /                   \\  |         ",
 "                          \\  |                     '\\'         ",
 "                           `~'                                 "
 ]
