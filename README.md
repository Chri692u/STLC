Typed-Lambda-Calculus REPL:
1. load Main in ghci *ghci Main.hs*
2. call the function *main*
3. Use the REPL or :run *filename* command
4. Or use the type command in the REPL with :type

#Examples
1. (\x:Bool . x) # abstraction
2. apply (\x:Int . x) 10 # application
3. apply (\x:Int -> Int . (apply x 20)) (\y:Int . y) # function types
4. apply (\x:Bool . x) 10 #Type mismatch
5. apply (\x:Int . y) 10 #Variable not in scope
6. apply True 10 #Not a function type