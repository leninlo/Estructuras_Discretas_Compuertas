



 data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred |
     Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred | Existe Nombre Pred

 data Term = Var Nombre | Fun Nombre [Term]
 type Nombre = String

   vars :: Pred -> [Nombre]
   vars PTrue = []
   vars PFalse = []
   vars (Predicado _ term) = (concatMap aux x)
   vars (Neg x) = vars x

   aux :: Term -> [Nombre]
   aux (Var x) = [x]
   aux (Fun _ x) = concat Map (aux x)
  
  {-libres :: Pred -> [Nombre]
  libres PTrue = []
  libres PFalse = []
  libres (Predicado n t) = concatMap aux  t
  libres (Neg x) = libres x
  libres (Conj x y) = libres x ++ libres y
  libres (Disy x y) = libres x ++ libres y

  libres (PTodo n t) = --if n pertenece a t then delete t de n else vars t

  equiv :: Pred -> Pred
  equiv PTrue = PTrue
  equiv PFalse = PFalse
  equiv (Predicado n x) = Predicado n x
  equiv (Neg x) = Neg (equiv x)

  equiv (Neg (PTodo x p)) = Existe x (equiv (Neg p))
  equiv (Existe x p) = PTodo x(equiv (Neg p))

  cuanti :: Pred -> Integer
  cuanti PTrue = 0
  cuanti PFalse = 0
  cuanti (Predicado x y) = cuanti(auxC x)
  cuanti (Neg p ) = cuanti p
  cuanti (Disy x y) = cuanti x +cuanti y
  cuanti (PTodo p q) = 1 + cuanti q

  auxC :: Term -> Integer
  auxC (Var x) = 0
  auxC (Fun  x y) = auxC y-}
  
  
  



                                                      


          


