# chairs are fruits

```purs
type FruitsRow = ( apple :: Int, grenade :: String, chair :: Boolean )

isThisFruit :: forall l result
   . LabelInRow l FruitsRow result
  => IsBoolean result
  => IsSymbol l
  => SProxy l
  -> String
isThisFruit sp =
  if test
    then "Yes, " <> fruit <> " is a bonafide fruit."
    else "No, " <> fruit <> " ain't no god damn fruit."
  where
    fruit = reflectSymbol sp
    test = reflectBoolean (BProxy :: _ result)

main :: Effect Unit
main = do
  traverse_ log
    [ isThisFruit (SProxy :: _ "wheels")
    , isThisFruit (SProxy :: _ "chair")
    ]
  -- No, wheels ain't no god damn fruit.
  -- Yes, chair is a bonafide fruit.
```

```purs
type Fruits = { apple :: Int, grenade :: String, chair :: Boolean }

fruits :: Fruits
fruits = { apple: 1, grenade: "string", chair: false }

-- inferred:
leaves :: BProxy B.False
leaves = labelInRow (SProxy :: _ "leaf") fruits

-- inferred:
chairs :: BProxy B.True
chairs = labelInRow (SProxy :: _ "chair") fruits
```


```purs
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
```
