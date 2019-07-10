module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean as B
import Prim.RowList as RL
import Type.Data.Boolean (class If)
import Type.Data.Symbol as Symbol
import Type.Prelude (BProxy(..), SProxy(..))

class LabelInRow (l :: Symbol) (r :: # Type) (result :: B.Boolean)
  | l r -> result

instance labelInRowInst ::
  ( RL.RowToList r rl
  , RowListHasLabel l rl result
  ) => LabelInRow l r result

class RowListHasLabel (l :: Symbol) (rl :: RL.RowList) (result :: B.Boolean)
  | l rl -> result

instance rowListHasLabelNil :: RowListHasLabel l RL.Nil B.False
else instance rowListHasLabelTrue ::
  ( Symbol.Equals l name equals
  , If equals
      (BProxy B.True)
      (BProxy else_)
      (BProxy result)
  , RowListHasLabel l tail else_
  ) => RowListHasLabel l (RL.Cons name ty tail) result

labelInRow :: forall l r result
   . LabelInRow l r result
  => SProxy l
  -> { | r }
  -> BProxy result
labelInRow _ _ = BProxy

type Fruits = { apple :: Int, grenade :: String, chair :: Boolean }

fruits :: Fruits
fruits = { apple: 1, grenade: "string", chair: false }

-- inferred:
leaves :: BProxy B.False
leaves = labelInRow (SProxy :: _ "leaf") fruits

-- inferred:
chairs :: BProxy B.True
chairs = labelInRow (SProxy :: _ "chair") fruits

main :: Effect Unit
main = do
  log "🍝"
