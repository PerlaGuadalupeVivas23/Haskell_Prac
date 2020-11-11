main = do
 regreso True
regreso seguir = do
    if seguir
        then do
        putStrLn "==================================="
        putStrLn "---------------M E N U-------------"
        putStrLn "==================================="
        putStrLn ("1.-Fibonacci\n2.-Presentar Numeros del 1-10\n3.-Factorial\n4.-Desaparece Numeros\n5.-Palindromos\n6.-Menu de Calculadora\n7.-Salir\n\n")
        opcion <- getLine

        case opcion of
            "1" -> do
                   putStrLn "Posicion:" 
                   n <- getLine
                   let nInt = read n::Int
                   fibo nInt
            "2" -> do
                 numeros 1

            "3" -> do
                    putStrLn("Dame un numero")
                    numero <-getLine
                    print ("factorial es:"++ show (factorial(read numero)))
                    regreso True
            "4" -> do
                    arreglo [0,1,2,3,4,5,6,7,8,9,10]
                    regreso True
            "5" -> do
                putStrLn ("ingresa frase:")
                palabra <- getLine
                palin palabra
            "6" -> do
                    putStrLn "------------CALCULADORA-----------"
                    putStrLn "1.- Suma"
                    putStrLn "2.- Resta"
                    putStrLn "3.- Multiplicacion"
                    putStrLn "4.-Division"
                    putStrLn("elige la operacion a realizar") 
                    n <- getLine
                    casos n
            "7" -> do
                regreso False
            _ -> print("Opcion invalida")
          
    else
         putStrLn "Estamos fuera del menu"
------------------------------------------------------------------

fibo nInt= do
    let arreglo = ["0", "11" , "1", "34" ,"3","5","66", "7","23","10"]

    if(nInt<= 9)
        then do
            print (arreglo !! nInt)
            regreso True
    else do
        regreso True

--------------------------------------------------------------------
numeros n = do
    if n<=10
        then do
            print n 
            numeros (n+1)
      else do
            regreso True
-----------------------------------------------------------------
factorial n= if n==0 then 1
            else n*factorial(n-1)
----------------------------------------------------------------
arreglo arr= 
    if null arr
        then 
            print("-")
    else do
        print (arr)
        arreglo(init arr)
--------------------------------------------------------------
palin palabra= do
    let res = palabra== reverse palabra

    if res==True
        then do
            putStrLn ("Palindromo")
            regreso True
    else do
        putStrLn ("No lo es")
        regreso True
--------------------------------------------------------------------

casos  n= do
    case n of 
        "1" ->suma
        "2" ->resta
        "3" ->multiplicacion
        "4" ->division
        _ -> print ("Opcion invalida")

suma = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a+b))
    regreso True

resta = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a-b))
    regreso True

multiplicacion = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a*b))
    regreso True

division = do
    putStrLn("numero 1: ")
    a <- readLn    
    putStrLn("numero 2: ")
    b <- readLn
    print ("El resultado es "++ show(a/b))
    regreso True

----------------------------------------------------------------
