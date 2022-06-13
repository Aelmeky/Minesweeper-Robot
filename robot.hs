type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState


up:: MyState -> MyState
up S((x,y) l s o) | (x+1<5) = S((x,y) l "up" (S(x+1,y) l "" Null))) 
		| otherwise = Null

down:: MyState -> MyState
down S((x,y) l s o) | (x-1>0) = S((x,y) l "down" (S(x-1,y) l "" Null))) 
		| otherwise = Null

left:: MyState -> MyState
left S((x,y) l s o) | (y-1>0) = S((x,y) l "left" (S(x,y-1) l "" Null))) 
		| otherwise = Null

right:: MyState -> MyState
right S((x,y) l s o) | (y+1<5) = S((x,y) l "right" (S(x,y+1) l "" Null))) 
		| otherwise = Null

collect:: MyState -> MyState
collect S((x,y) l s o) | (collecthelper (x,y) l ) = S((x,y) l "collect" (S(x,y+1) l "" Null))) 
		| otherwise = Null

collecthelper (x,y) [] = False
collecthelper (x,y) ((x1,x2):l) | (x==x1 && y==y1) = True
				| collecthelper (x,y) l


