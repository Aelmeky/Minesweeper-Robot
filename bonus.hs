type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] deriving (Show,Eq)


seekx 0 = []
seekx dx | (dx > 0) = "down":(seekx (dx-1))
		| otherwise =  "up":(seekx (dx+1))

seeky 0 = []		
seeky dy | (dy > 0) = "right":(seeky (dy-1))
		| otherwise =  "left":(seeky (dy+1))

		

seek (xi,yi) (xf,yf) = (seekx (xf-xi)) ++ (seeky (yf-yi))++["collect"]

		
solveHelper (S (x,y) []) sol = sol

solveHelper (S (x,y) (h:t)) sol = solveHelper (S h t) (sol ++ (seek (x,y) h))

		
solve :: Cell->[Cell]->[String]

solve pos mines = solveHelper (S pos mines) []

		
			
			