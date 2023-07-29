import Lib


main =let tree1 = Node 1 (Cons (Node 2 Nil)
                (Cons (Node 3
                    (Cons (Node 4 Nil)Nil))Nil
                       )) 
      in do
      putStr $ "A few sanity tests:\n\n"
      putStr $ "sum(Cons 1 (Cons 2 Nil))=" ++ (show $ Lib.sum (Cons 1 (Cons 2 Nil))) ++ "\n\n"
      putStr $ "append (Cons 1 (Cons 2 Nil)) (Cons 3 Nil)=" ++ (show $ Lib.append (Cons 1 (Cons 2 Nil)) (Cons 3 Nil)) ++ "\n\n"
      putStr $ "doubleall (Cons 1 (Cons 2 Nil))="++(show $ doubleall (Cons 1 (Cons 2 Nil))) ++"\n\n"
      putStr $ "sumTree tree1="++(show $ sumTree tree1)++"\n\twhere tree1="++(show $ tree1)++"\n\n"
      putStr $ "labels tree1="++(show $ labels tree1)++"\n\twhere tree1="++(show $ tree1)++"\n\n"
      putStr $ "sqrt 1 0.001 2="++(show $ Lib.sqrt 1 0.001 2)++"\n\twhere starting value=1,eps=0.001,n=2\n\n"
      putStr $ "sqrt 1 0.001 3="++(show $ Lib.sqrt 1 0.001 3)++"\n\twhere starting value=1,eps=0.001,n=3\n\n"
      putStr $ "relativeSqrt 1 0.001 2="++(show $ Lib.relativeSqrt 1 0.001 2)++"\n\twhere starting value=1,eps=0.001,n=2\n\n"
      putStr $ "relativeSqrt 1 0.001 3="++(show $ Lib.relativeSqrt 1 0.001 3)++"\n\twhere starting value=1,eps=0.001,n=3\n\n"
      putStr $ "The derivative of f(x)=x*x at x=1:\n within 0.001 (differentiate 0.1 (\\x -> x*x) 1)="++(show $ within 0.001 (differentiate 0.1 (\x->x*x) 1))++"\n\n"
      putStr $ "The integration of f(x)=x from 0 to 1:\n within 0.001 (integrate (\\x->x) 0 1)=" ++ (show $ within 0.001 (integrate (\x->x) 0 1))++"\n\n"
