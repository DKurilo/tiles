# tiles

It's a solver for Fifteen game or any other tiles game.
Input (numbers and X for hole):

```
1,2,3
4,5,6
7,8,X
Ctrl+D to stop input
```
or
```
1,2,3,4
5,6,7,8
9,10,11,12
13,14,15,X
Ctrl+D to stop input
```
or
```
1,7,9,5,10
11,2,22,4,3
12,23,24,8,13
21,17,15,X,14
6,16,18,19,20
Ctrl+D to stop input
```
result here is:
```
[L,U,U,U,L,D,R,R,R,U,L,D,L,U,R,D,D,R,D,L,L,U,L,D,D,L,U,R,D,R,U,U,L,D,D,R,U,U,U,L,D,L,U,R,R,D,R,R,D,D,L,U,U,U,L,L,D,R,U,R,D,L,L,U,R,R,D,D,L,L,D,R,R,R,U,L,D,L,U,R,R,D,L,L,U,R,D,L,L,U,R,R,D,R]
```
It means you need to move hole Left, Up, Up and so on. L for Left, U for Up, R for Right, D for Down

You can also try something like:
```
X,12,9,13
15,11,10,14
3,7,2,5
4,8,6,1
```
this combination require at least 80 turns to solve it. I found it [here](http://kociemba.org/themen/fifteen/fifteensolver.html)
My result is 88 turns, good enough when using Manhattan distance:
[D,D,R,U,R,U,L,L,D,R,R,D,R,U,U,L,D,D,D,L,U,U,R,D,D,R,U,L,D,L,L,U,R,D,L,U,U,R,U,R,D,L,D,R,U,R,D,L,U,U,L,D,D,R,D,L,U,L,U,R,R,U,L,L,D,D,D,R,U,U,R,R,U,L,L,D,D,R,D,R,U,U,L,D,L,D,R,R]
It takse some time to find result, so be patient.
