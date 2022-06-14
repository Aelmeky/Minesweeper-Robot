type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] deriving (Show,Eq)

seekx :: Int -> [String]
seekx 0 = []
seekx dx | (dx > 0) = "down":(seekx (dx-1))
		| otherwise =  "up":(seekx (dx+1))

seeky :: Int -> [String]
seeky 0 = []		
seeky dy | (dy > 0) = "right":(seeky (dy-1))
		| otherwise =  "left":(seeky (dy+1))

seek :: Cell -> Cell -> [String]
seek (xi,yi) (xf,yf) = (seekx (xf-xi)) ++ (seeky (yf-yi))++["collect"]



solveHelper (S (x,y) []) sol = sol

solveHelper (S (x,y) (h:t)) sol = solveHelper (S h t) (sol ++ (seek (x,y) h))

		
solve :: Cell->[Cell]->[String]

solve pos mines = solveHelper (S pos (sortMines mines [])) []


compareTo :: Cell -> Cell -> Int

compareTo (a,b) (c,d) | (not ((a-c)==0)) = a-c
						| otherwise = b-d


insertSort :: Cell -> [Cell] -> [Cell]

insertSort x [] = [x]					
insertSort x (h:t) | ( (compareTo x h) <= 0) = x:h:t
					|otherwise = h:(insertSort x t)
					
sortMines :: [Cell] -> [Cell] -> [Cell]
sortMines [] sol = sol
sortMines (h:t) sol = sortMines t (insertSort h sol)







