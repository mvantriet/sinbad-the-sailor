import Data.List

calcToll :: String -> String -> Int -> IO ()
calcToll startpoint endpoint spoons =
    let
        --construct a table row with startpoint cost as spoons distance and
        --all other vertices with cost infinity
        baserow = (Tollcost spoons, startpoint, startpoint) : [(Inf, n, "") | n <- graphnodes, n /= startpoint]
        table = toll endpoint graphnodes baserow
        --cheapest route has been calculated, find distance in the table
        (Tollcost spoontoll) = endValue endpoint table
    in
        -- Output the toll info to Sinbad!
        print $ "Sinbad needs to leave with at least: " ++ (show spoontoll) ++ " spoons"

-- Find shortest paths.
toll :: String -> [String] -> [(Tollcost , String , String)] -> [(Tollcost , String, String)]
toll endpoint remainingNodes table =
    let
        -- extract vertice with smallest cost
        minV@(_, minVlabel, _) = choose_minv remainingNodes table 
        newRemainingNodes = delete minVlabel remainingNodes 
        newTable = neighBourTable newRemainingNodes minV table
    in
        if (minVlabel == endpoint)
        -- were done
        then
            newTable
        else
            toll endpoint newRemainingNodes newTable
--
getCheapRoute :: (Tollcost , String , String) -> (Tollcost , String , String) -> (Tollcost, String , String)
getCheapRoute curMinV@(curMinVspoons, curMinVlabel, curMinVadj) nextV@(nextVspoons, nextVlabel, nextVadj) =
    let
        cheaperSpoons = addTollCosts curMinVspoons (getEdgeCost curMinV nextV)
    in
        if nextVspoons < cheaperSpoons -- is route via nextV faster?
        then
            nextV
        else
            (cheaperSpoons, nextVlabel, curMinVlabel)

-- Find edges from minV -> every element in remainingNodes
neighBourTable :: [String] -> (Tollcost , String , String) -> [(Tollcost , String, String)] -> [(Tollcost , String , String)]
neighBourTable _ _ [] = [] -- base case

neighBourTable remainingNodes minV@(minVspoons , minVlabel , _) (nextV@(nextVspoons, nextVlabel, _) : table) =
    if (elem nextVlabel remainingNodes) -- have we looked at nextV
    then
        let
            newNextV = getCheapRoute minV nextV
        in
            newNextV : neighBourTable remainingNodes minV table
    else
        nextV : neighBourTable remainingNodes minV table

addTollCosts :: Tollcost -> Tollcost -> Tollcost
addTollCosts (Tollcost x) (Tollcost y) = Tollcost(x + y)
addTollCosts Inf _ = Inf
addTollCosts _ Inf = Inf

choose_minv :: [String] -> [(Tollcost , String , String)] -> (Tollcost, String, String)
choose_minv remainingNodes [curMinV@(_, curMinVlabel, _)] =
    if (elem curMinVlabel remainingNodes)
    then
        curMinV
    else
        (Inf, "Error", "")

choose_minv remainingNodes (nextMinV@(nextMinVspoons, nextMinVlabel, _) : table) =
    let
        curMinV@(curMinVSpoons , curMinVlabel , _) = choose_minv remainingNodes table
    in
        if (nextMinVspoons < curMinVSpoons && (elem nextMinVlabel remainingNodes))
        then nextMinV
        else curMinV

endValue :: String -> [(Tollcost, String, String)] -> Tollcost
endValue endpoint ((spoonCost, vLabel, _) : table)
    | vLabel == endpoint = spoonCost
    | otherwise = endValue endpoint table

getEdgeCost :: (Tollcost , String , String) -> (Tollcost , String , String) -> Tollcost
getEdgeCost curMinV@(curMinVspoons , curMinVlabel , curMinVadj) nextV@(nextVspoons ,nextVlabel, nextVadj) =
    let
        tollCur = calcTollToTown curMinVspoons
    in
        if (((graphedges curMinVlabel nextVlabel) == Tollcost 1) || ((graphedges nextVlabel curMinVlabel) == Tollcost 1))
        then Tollcost 1
        else if (((graphedges curMinVlabel nextVlabel) == Tollcost 2) || ((graphedges nextVlabel curMinVlabel) == Tollcost 2))
            then tollCur
            else Inf

calcTollToTown :: Tollcost -> Tollcost
calcTollToTown (Tollcost x) = Tollcost (( div (x + ((div x 20))) 20 ) +1)

data Tollcost = Tollcost Int | Inf
instance Eq Tollcost where
    Inf == _ = False
    _ == Inf = False
    (Tollcost x) == (Tollcost y) = x == y

instance Ord Tollcost where
    Inf <= _ = False
    _ <= Inf = True
    (Tollcost x) <= (Tollcost y) = x <= y

graphedges :: String -> String -> Tollcost
graphedges "x" "d" = Tollcost 2
graphedges "x" "c" = Tollcost 2
graphedges "d" "a" = Tollcost 2
graphedges "c" "b" = Tollcost 1
graphedges "b" "a" = Tollcost 1

graphedges _ _ = Inf
graphnodes :: [String]
graphnodes = ["x", "d", "c", "b", "a"]

-- Main method, currently hardcoded according to the example in the challenge explanation
main :: IO ()
main = calcToll "a" "x" 39