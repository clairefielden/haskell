
# Universal Turing Machine 

A Universal Turing machine (UTM) implementation in Haskell. 

## The Universal Turing Machine

### BEHAVIOUR

Lambda calculus is Turing complete. A universal function accepts a program `f` and input `x` and simulates the computation of `f(x)`.

The Universal Turing Machine will accept the decription of another Machine, `M`, and an input tape, `t`, and returns the reuslt of running `M(t)`.

### ALPHABET

A Universal Turing Machine has the following language:
```
                  UTM = { ⟨M, t⟩ | State } 
```
The UTM will determine whether the language can be computed by M. This is determined by the state.

```bash
# a > b
>   main = print $ runMachine turingMachine "_11101_"
>   FINAL STATE: Accept
    This Language is Turing Recognizable
    Result: 2
```

```bash
# a < b
>   main = print $ runMachine turingMachine "_10111_"
>   FINAL STATE: Reject
    Argument 1 should be greater than Argument 2
    This Language is not Turing Recognizable
```

```bash
# a not unary
>   main = print $ runMachine turingMachine "_xyz0111_"
>   FINAL STATE: Reject
    Argument must consist of only 1
    This Language is not Turing Recognizable
```

```bash
# let a = concatMap (replicate 50) [1]
# should not be more than 32 bits
# let b = []
# should be more than 0 bits
>   main = print $ runMachine turingMachine a ++ b
>   FINAL STATE: Reject
    Argument 1 is of incorrect length!
    Argument 2 is of incorrect length!
    This Language is not Turing Recognizable
```

### STATES

The UTM is represented by a monadic computation.

``` haskell
import Control.Monad.State
type UTM a b = State (Machine a) b
```
Any computation performed by the UTM will be structured in terms of the values and sequences of the input Turing Machine. It will have the following outputs:

| Input Machine | Input Tape | TM State | Result | Recognizable
| ---- | ----- | -------- | ------ | ----- 
| `turing.txt` |  `_1101_`  |*Accept*| 1 | True 
| `turing.txt`  |  `_1011_`  | *Reject* | *null* | False


### INSTRUCTION FORMAT

The Universal Turing Machine reads in a textfile containing a quintuple, (Q, Σ, δ, q0, h), representing a Turing Machine. 

``` haskell
loadMachine "myTuring.txt" >>= runMachine
```

The UTM uses one tape as the input to the Turing Machine, which will be fed into the UTM. If the UTM **Halts** and **Accepts**, the input language is `Turing Recognizable`. If the computation is `undecidable` or the input Machine **Halts** and **Rejects**, the input tape is not `Turing Recognizable`.

### MACHINE FORMAT

```sh
# Argument 1: A Turing Machine

cat myTuring.txt
    ; Initial state: Start
    ; Final states: Halt, Accept
    ; Blank symbol: _
    ; No-op symbol: *
    
    ;Subtractor

    Start _ _ r Start
    Start 1 . r ScanRight
    ScanRight 1 1 r ScanRight
    ScanRight 0 0 r ArgA
    ArgA . . r ArgA
    ArgA _ _ r Accept
    ArgA 1 . l ScanLeft
    ScanLeft . . l ScanLeft
    ScanLeft 0 0 l ArgB
    ArgB 1 1 l ArgB
    ArgB . . r Start
    Start 0 0 r Subtract
    Subtract . _ r Subtract
    Subtract 1 1 r Halt
```

```bash
//Argument 2: 2 unary numbers from the user

> Argument 1:
1
> Argument 2:
111
> Tape: 
_11101_
```
![input](https://github.com/clairefielden/haskell/assets/98583663/f660bf29-180e-4f8a-8c91-42df732899cc)

## The Turing Machine

#### BEHAVIOUR
The Turing Machine will take an input and perform unary subtraction. Unary subtraction computes the difference in the number of characters between argument 1 and argument 2.
> Example : `111 - 11 ` would give `1`

#### ALPHABET
The alphabet of the input will be composed of 3 characters:
- An argument may only consist of `1`'s
- Each argument is separated by a `0`
- The blank symbol `_` indicates the start and end of the tape
- The machine will write `.` , indicating a "skip", which cannot be input on the tape

#### STATES
```haskell
data Action = Halt | Start | ScanRight | ScanLeft | ArgA | ArgB | Subtract | Accept
```
The Turing Machine halts if no instruction for current state and input symbol exists. Otherwise, it begins with a read/write head positioned at the leftmost input symbol. The machine will accept if there is are only bits on the left side of 0 remaining on the tape.

#### Example :
| Input Tape | Output Tape | Final State |
| ---- | ----- | -------- |
| `_11101_`  |  `_..10.__`  |**ACCEPT**|
| `_10111_`  |  `_.0_11_`  | **HALT** |

#### INSTRUCTION FORMAT
```haskell
transition :: Action -> Maybe Symbol -> (Action, Symbol, Direction)
```
`Instructions` take a `State` and a `Symbol` as input and map the machine to its next `transition`. This is based on the transition table.

![table](https://github.com/clairefielden/haskell/assets/98583663/6e21b11e-c30c-45f1-90f8-89aceb13fb20)


#### MACHINE FORMAT:

```
A Turing Machine is a quintuple (Q, Σ, δ, q0, h) where 
1. Q is a finite set of states 
2. Σ is the alphabet (the function computed by the Turing Machine will go from Σ∗ to Σ∗)
3. δ is the next move function, indicating the direction the tape head will go
4. q0 ∈ Q is the start state
5. h ∈ Q is the halting state 

```

The instruction set can also be expressed using a State Transition Diagram. Please note that for the sake of reaability, blanks are represented by `B` and skips are represented by `*`.

![fsm](https://github.com/clairefielden/haskell/assets/98583663/1833fde2-b738-463c-842e-a92c84f50f19)

## USAGE
1. To run the Turing Machine:
> `> ghci Turing.hs` <br>
> `> main "111" "1" ` <br>
2. To run the UTM
> `> ghci UTM.hs` <br>
> `> main` <br>

#### Example Turing Machine
![turing](https://github.com/clairefielden/haskell/assets/98583663/b2d3dd8e-16d1-48ca-8930-ec3facdbc947)

#### Example UTM

![UTM](https://github.com/clairefielden/haskell/assets/98583663/cbc45ee2-e3fc-4e0d-b7d9-c174098d84ce)


## Author
Claire Fielden
FLDCLA001@myuct.ac.za

