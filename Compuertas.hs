
  module Compuertas where
  data Bit = Cero | Uno deriving Eq

  instance Show Bit where
   show Cero = "0"
   show Uno = "1"

  compuertaAND :: Bit -> Bit -> Bit
  compuertaAND Uno Uno = Uno
  compuertaAND _ _ = Cero

  compuertaOR :: Bit -> Bit -> Bit
  compuertaOR Cero Cero = Cero
  compuertaOR _ _ = Uno

  compuertaNOT :: Bit -> Bit
  compuertaNOT a = if a == Cero then Uno else Cero

  compuertaNAND :: Bit -> Bit -> Bit
  compuertaNAND Uno Uno = Cero
  compuertaNAND _ _ = Uno

  compuertaNOR :: Bit -> Bit -> Bit
  compuertaNOR Cero Cero = Uno
  compuertaNOR _ _ = Cero

  compuertaXOR :: Bit -> Bit -> Bit
  compuertaXOR a b = if a == b then Cero else Uno

  compuertaXNOR :: Bit -> Bit -> Bit
  compuertaXNOR a b = if a == b then Uno else Cero

  halfAdder :: Bit -> Bit -> (Bit, Bit)
  halfAdder a b = (compuertaXOR a b, compuertaAND a b)

  fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
  fullAdder a b c = (compuertaXOR (compuertaXOR a b) c, compuertaOR (compuertaAND a b) (compuertaAND c (compuertaXOR a b)))

  flipFlopRS :: Bit -> Bit -> Bit -> Bit
  flipFlopRS a b c = compuertaOR a (compuertaAND (compuertaNOT b) c)

  flipFlopJK :: Bit -> Bit -> Bit -> Bit
  flipFlopJK q j k = compuertaOR (compuertaAND (compuertaNOT q) j) (compuertaAND (compuertaNOT k) q)

  flipFlopD :: Bit -> Bit -> Bit
  flipFlopD estado dato = dato
  
  flipFlopT :: Bit -> Bit -> Bit
  flipFlopT estado dato = compuertaXOR estado dato
