{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fungeoid where

import Control.Monad.Trans.State
import Control.Monad.Trans (lift)

import Action
import Commutative


data Fungeoid g h programType opType s = FS { orientation :: g, vector :: h, coordinate :: h,  program :: programType, getOp :: h -> programType -> opType, extraState :: s }

type Morell m g h programType opType s c = StateT (Fungeoid g h programType opType s) m c

tick :: (Semigroup h, Action g h, Monad m) => Morell m g h p op s op
tick = do
  (FS o v c p f s) <- get
  let newCoord = (act o v <> c)
  put (FS o v newCoord p f s)
  return (f newCoord p)

turn :: (Commutative g, Monad m) => g -> Morell m g h p op s ()
turn = turnLeft

turnLeft :: (Semigroup g, Monad m) => g -> Morell m g h p op s ()
turnLeft g = do
  (FS o v c p f s) <- get
  put (FS (g <> o) v c p f s)

turnRight :: (Semigroup g, Monad m) => g -> Morell m g h p op s ()
turnRight g = do
  (FS o v c p f s) <- get
  put (FS (o <> g) v c p f s)

move :: (Commutative h, Monad m) => h -> Morell m g h p op s ()
move = moveLeft

moveLeft :: (Semigroup h, Monad m) => h -> Morell m g h p op s ()
moveLeft h = modify $ \(FS o v c p f s) -> (FS o v (h <> c) p f s)

moveRight :: (Semigroup h, Monad m) => h -> Morell m g h p op s ()
moveRight h = modify $ \(FS o v c p f s) -> (FS o v (c <> h) p f s)

accelerate :: (Commutative h, Monad m) => h -> Morell m g h p op s ()
accelerate = accelerateLeft

accelerateLeft :: (Semigroup h, Monad m) => h -> Morell m g h p op s ()
accelerateLeft h = modify $ \(FS o v c p f s) -> (FS o (h <> v) c p f s)

accelerateRight :: (Semigroup h, Monad m) => h -> Morell m g h p op s ()
accelerateRight h = modify $ \(FS o v c p f s) -> (FS o (v <> h) c p f s)

skipLeft :: (Semigroup h, Action g h, Monad m) => Morell m g h p op s ()
skipLeft = modify (\(FS o v c p f s) -> FS o v (act o v <> c) p f s)

skipRight :: (Semigroup h, Action g h, Monad m) => Morell m g h p op s ()
skipRight = modify (\(FS o v c p f s) -> FS o v (c <> act o v) p f s)

orient :: Monad m => g -> Morell m g h p op s ()
orient newOrientation = modify $ \(FS _ v c p f s) -> FS newOrientation v c p f s

direct :: Monad m => h -> Morell m g h p op s ()
direct newVector = modify $ \(FS o _ c p f s) -> FS o newVector c p f s

translocate :: Monad m => h -> Morell m g h p op s ()
translocate newCoord = modify $ \(FS o v _ p f s) -> FS o v newCoord p f s

inspect :: Monad m => h ->  Morell m g h p op s op
inspect coord = get >>= \(FS _ _ _ program f _) -> return (f coord program)

reflect :: Monad m => (prog -> prog) -> Morell m g h prog op s ()
reflect fun = modify $ \(FS o v c p f s) -> FS o v c (fun p) f s

getState :: Monad m => Morell m g h prog op s s
getState = gets extraState

modState :: Monad m => (s -> s) -> Morell m g h p op s ()
modState fun = modify $ \(FS o v c p f s) -> FS o v c p f (fun s)

putState :: Monad m => s -> Morell m g h p op s ()
putState s = modify $ \(FS o v c p f _) -> FS o v c p f s
