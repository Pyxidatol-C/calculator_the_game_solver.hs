# calculator_the_game_solver.hs

A haskell solver for [*Calculator The Game*](https://apps.apple.com/us/app/calculator-the-game/id1243055750)

## Gallery

### LEVEL: 42

<img
    alt="initial value: 0; goal: -13; moves: 4; buttons: +3, -7, +/-]"
    src="gallery_assets/lvl_42.PNG"
    width="256"
/>

```hs
main :: IO ()
main =
    let v0      = 0
        goal    = -13
        nbMoves = 4
        ops     = [Add 3, Sub 7, Neg]
        f       = id
        -- Change the parameters above according to the lvl
        res     = solution v0 goal nbMoves ops f
    in  print res
```

```console
$ ./calc_the_g
[+3,+3,+/-,-7]
```

### LEVEL: 155

<img 
    alt="initiial value: 9; goal: 3001, moves: 9; buttons: 39 => 93, /3, Store, 31 => 00"
    src="gallery_assets/lvl_155.PNG"
    width="256"
/>

```hs
main :: IO ()
main =
    let
        v0      = 9
        goal    = 3001
        nbMoves = 9
        ops =
            [Replace "39" "93", Div 3, StoreNew, StoreInsert, Replace "31" "00"]
        f   = id
        -- Change the parameters above according to the lvl
        res = solution v0 goal nbMoves ops f
    in
        print res
```

```console
$ ./calc_the_g
[Store,/3,Store,39 => 93,Store,Store,39 => 93,/3,31 => 00]
```

### LEVEL: 188

<img
    alt="initial value: 25; goal: 822; moves: 6; portal piping the 1st digit counting from the left to the 3rd digit; buttons: Mirror, 5, Store, <<"
    src="gallery_assets/lvl_188.PNG"
    width="256"
/>

```hs
main :: IO ()
main =
    let v0      = 25
        goal    = 822
        nbMoves = 6
        ops     = [Mirror, Insert 5, StoreNew, StoreInsert, Delete]
        f       = portal 3 1
        -- Change the parameters above according to the lvl
        res     = solution v0 goal nbMoves ops f
    in  print res
```

```console
$ ./calc_the_g
[5,Mirror,Store,Store]
```

### LEVEL: 194

<img
    alt="initial value: 333; goal: 123; moves: 4; buttons: 1, 3, /2, [+]1]"
    src="gallery_assets/lvl_194.PNG"
    width="256"
/>

```hs
main :: IO ()
main =
    let v0      = 333
        goal    = 123
        nbMoves = 4
        ops     = [Insert 1, Insert 3, Div 2, IncrementFuckingBloodyMeta 1]
        f       = portal 3 0
        -- Change the parameters above according to the lvl
        res     = solution v0 goal nbMoves ops f
    in  print res
```

```console
$ ./calc_the_g
[3,[+]1,/3,2]
```
