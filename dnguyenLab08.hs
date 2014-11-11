-- Dang Minh Nguyen
-- COSC 304
-- Lab 08
-- 11/11/2014


-- Stuff from lab 07

spec1 (0, '#') = (0, '#', 'l')
spec1 (0, 'a') = (1, 'a', 'l')
spec1 (0, 'b') = (1, 'a', 'l')
spec1 (0, 'c') = (1, 'a', 'l')
spec1 (0, 'd') = (1, 'a', 'l')
spec1 (1, '#') = (2, '#', 'r')
spec1 (1, 'a') = (1, 'a', 'l')
spec1 (1, 'b') = (1, 'a', 'l')
spec1 (1, 'c') = (1, 'a', 'l')
spec1 (1, 'd') = (1, 'a', 'l')
spec1 (2, '#') = (100, '#', 'd')
spec1 (2, 'a') = (2, 'a', 'r')
spec1 (2, 'b') = (2, 'b', 'r')
spec1 (2, 'c') = (2, 'c', 'r')
spec1 (2, 'd') = (2, 'd', 'r')

val :: (String, Int) -> Char
val (a:rest, 0) = a
val (a:rest, n+1) = val (rest, n)

str1 = "abcd"

startstring str = "#" ++ str ++ "#"
startpos str = length str + 1

chstr (a:rest, ch, 0) = ch:rest
chstr (a:rest, ch, n+1) = a: (chstr (rest, ch, n)) 


move (str, state, ch, pos) = (newstr, newstate, newch, newpos)
                              where (newstate, oldch, direction) = spec1 (state, ch)
                                    newstr = chstr (str, oldch, pos)
                                    newpos  | direction == 'd' = pos
                                            | direction == 'l' = pos - 1
                                            | direction == 'r' = pos + 1
                                    newch = val (newstr, newpos)

run (str, state, ch, pos) = let (newstr, newstate, newch, newpos) = move (str, state, ch, pos)
                            in if newstate == 100
                                  then (newstr, newstate, newch, newpos)
                                  else run (newstr, newstate, newch, newpos)
                                  
startrun str = run (startstr, 0, val (startstr, pos), pos) 
                   where    startstr = startstring str
                            pos = startpos str
                            
                            
-- Question 1
newmove tmspec (str,state,ch,pos) = (newstr, newstate, newch, newpos)
                                    where   (newstate, oldch, direction) = tmspec (state, ch)
                                            newstr = chstr (str, oldch, pos)
                                            newpos  | direction == 'd' = pos
                                                    | direction == 'l' = pos - 1
                                                    | direction == 'r' = pos + 1
                                            newch = val (newstr, newpos)         

newrun tmspec (str, state, ch, pos) =   let (newstr, newstate, newch, newpos) = newmove tmspec (str, state, ch, pos)
                                        in if newstate == 100
                                            then (newstr, newstate, newch, newpos)
                                            else newrun tmspec (newstr, newstate, newch, newpos)                                         
                                            
newstartrun tmspec str =    newrun tmspec (startstr, 0, val (startstr, pos), pos) 
                            where   startstr = startstring str
                                    pos = startpos str                                           
                                    
{-
*Main> startrun str1
("#aaaa#",100,'#',5)
*Main> newstartrun spec1 str1
("#aaaa#",100,'#',5)
-}

                                    
-- Question 2, 3
spec2 (0, '#') = (1, '#', 'l')
spec2 (0, 'a') = (0, 'a', 'l')
spec2 (0, 'b') = (0, 'b', 'l')
spec2 (0, 'c') = (0, 'c', 'l')
spec2 (0, 'd') = (0, 'd', 'l')
spec2 (1, '#') = (2, '#', 'r')
spec2 (1, 'a') = (1, 'b', 'l')
spec2 (1, 'b') = (1, 'c', 'l')
spec2 (1, 'c') = (1, 'b', 'l')
spec2 (1, 'd') = (1, 'b', 'l')
spec2 (2, '#') = (100, '#', 'd')
spec2 (2, 'a') = (2, 'a', 'r')
spec2 (2, 'b') = (2, 'b', 'r')
spec2 (2, 'c') = (2, 'c', 'r')
spec2 (2, 'd') = (2, 'd', 'r')

spec3 (0, '#') = (1, '#', 'l')
spec3 (0, 'a') = (0, 'a', 'l')
spec3 (0, 'b') = (0, 'b', 'l')
spec3 (0, 'c') = (0, 'c', 'l')
spec3 (0, 'd') = (0, 'd', 'l')
spec3 (1, '#') = (2, '#', 'r')
spec3 (1, 'a') = (2, 'd', 'l')
spec3 (1, 'b') = (2, 'a', 'd')
spec3 (1, 'c') = (2, 'd', 'l')
spec3 (1, 'd') = (2, 'd', 'l')
spec3 (2, '#') = (100, '#', 'd')
spec3 (2, 'a') = (2, 'a', 'r')
spec3 (2, 'b') = (2, 'b', 'r')
spec3 (2, 'c') = (2, 'c', 'r')
spec3 (2, 'd') = (2, 'd', 'r')

{-
*Main> newstartrun spec2 str1
("#bcbb#",100,'#',5)
*Main> newstartrun spec3 str1
("#abcd#",100,'#',5)
*Main> newstartrun spec3 "ccad"
("#ccad#",100,'#',5)
-}


-- Question 4
spec4 (0, '#')  = (100, 'a', 'r')
spec4 (0, x)    = (100, x, 'r')

{- 
Result
*Main> newstartrun spec4 "aaa"
("#aaaa",100,*** Exception: dnguyenLab08.hs:(26,0)-(27,32): Non-exhaustive 
patterns in function val

This happens when the machine move right beyond the '#' we added
to the end of "aaa". At this point, the run function tried to 
call val on a position that is beyond the string, without adding
'#' to the end of it.
-}


-- Question 5
move2 tmspec (str,state,ch,pos) = (newstr, newstate, newch, newpos)
                                    where   (newstate, oldch, direction) = tmspec (state, ch)
                                            
                                            newpos  | direction == 'd' = pos
                                                    | direction == 'l' = pos - 1
                                                    | direction == 'r' = pos + 1
                                            newstr = if newpos >= (length str) 
                                                        then (chstr (str, oldch, pos)) ++ "#"
                                                        else chstr (str, oldch, pos)
                                            newch = val (newstr, newpos)
                                                  
                                                     
run2 tmspec (str, state, ch, pos) =   let (newstr, newstate, newch, newpos) = move2 tmspec (str, state, ch, pos)
                                        in if newstate == 100
                                            then (newstr, newstate, newch, newpos)
                                            else run2 tmspec (newstr, newstate, newch, newpos)                                         
                                            
startrun2 tmspec str =  run2 tmspec (startstr, 0, val (startstr, pos), pos) 
                        where   startstr = startstring str
                                pos = startpos str         

{-
*Main> startrun2 spec4 "aaa"
("#aaaa#",100,'#',5)
*Main> startrun2 spec4 str1
("#abcda#",100,'#',6)
-}                                