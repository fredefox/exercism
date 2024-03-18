module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Char
import Data.IORef

type Robot = IORef Int
type RunState = Int

initialState :: RunState
initialState = 0

incr :: StateT RunState IO Int
incr = State.get <* State.modify succ

mkRobot :: StateT RunState IO Robot
mkRobot = incr >>= State.lift . newIORef

resetName :: Robot -> StateT RunState IO ()
resetName r = do
  n <- incr
  State.lift $ writeIORef r n

robotName :: Robot -> IO String
robotName = fmap rb . readIORef

dig :: Int -> Char
dig = intToDigit

ch :: Int -> Char
ch = toEnum . (+ 65)

rb :: Int -> String
rb r0 = ch a0 : ch a1 : dig a2 : dig a3 : dig a4 : []
  where
  (r1, a0) = r0 `divMod` 26
  (r2, a1) = r1 `divMod` 26
  (r3, a2) = r2 `divMod` 10
  (r4, a3) = r3 `divMod` 10
  (_5, a4) = r4 `divMod` 10
