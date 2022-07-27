#Typed-Lambda-Calculus
#REPL
In terminal:
1. *ghci Main.hs*
2. *main*
3. Use the REPL or :run *filename* command

#Syntax
Expr = Int
     | Let String Expr Expr
     | Var String
     | Lambda String Expr
     | App Expr Expr
     | Binary Operation Expr Expr
     
Operation = Add
          | Sub
          | Mul
          | Div
          
#Examples
1. (\x. x) # abstraction
2. apply (\x. x) 10 # application
3. let y=10 in y #let bindings
4. 5+5 #Addition
5. 5-5 #Subtraction
6. 5/5 #Division
7. 5*5 #Multiplication
