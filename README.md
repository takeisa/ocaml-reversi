ocaml-reversi
=============

Reversi game that was created by using the OCaml.

I have been using minimax algorithm to thinking of the computer algorithm. 
But, the evaluation function of the board is very irresponsible.

## Sample terminal output ##

```
  a b c d e f g h
0                 
1                 
2         X       
3       O X O     
4       O O X     
5       O O O     
6                 
7                 

X:You [3]  O:Computer [7]

Turn 7 You

Input disk position [a1-h8] / Undo[u] / Quit[q] :
```

## Requirements ##

* The Core Standard Library

```sh
$ opam install core
```

* OMake

```sh
$ opam install omake
```


## Compile ##

```sh
$ omake
```

## Usage ##

```sh
$ ./reversi.opt
```


