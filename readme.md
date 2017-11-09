# Reversible

Symmetries occur in many different places and in many different forms. This is also true in code. With this package you can declare such symmetries up front. Whether or not that can be helpful is left to be explored.

## Simple examples

```
increment : ReversibleFunction Int Int
increment = add 1

apply increment 5 == 6
applyReverse increment 5 == 4
```

## Records

Getters and setters for records are also a sort of symmetry. Given a value for a record, you can define a reversible function as a helper to update a field. 

```
type alias Model = { counter : Int }

counter : Model -> ReversibleFunction Model Int
counter model =
    ReversibleFunction .counter (\value -> { model | counter = value })

updateValue counter (\value -> value + 1) { counter = 0 } == { counter = 1 }
```

The upsides and downsides are similar to those of evancz/focus but this allows more customizability. Take an examples with multiple fields in nested records.

```
type alias Physics a = { record | physics : a } 
type alias Velocity a = { record | velocity : a } 
type alias hasX a = { record | x : number } 
type alias hasY a = { record | y : number } 

updateVelocityXY : ((n,n) -> (n,n)) -> Physics a -> Physics a
velocityXY : Physics a -> ReversibleFunction (Physics a) (number,number)
velocityXY physics =
    ReversibleFunction
        (physics.velocity.x,physics.velocity.y)
        (\(x,y) -> { physics | velocity = { physics.velocity | x = x, y = y})

reset : Physics a -> Physics a
reset record =
    updateValue velocityXY (always (0,0)) record   
```
