-module(recursion).
-export([id/1,fib1/1,fib2/1,fib3/1,pieces/1,sum/2,perfect/1]).

id(X) -> X.

fib1(0) -> 0;
fib1(1) -> 1;
fib1(N) when N>0 -> fib1(N-1) + fib1(N-2).

pieces(0) -> 1;
pieces(N) when N>0-> pieces(N-1) + N.

sum(F, N) -> sum(F, N, 0).
sum(_, 0, P) -> P;
sum(F, N, P) -> sum(F,N-1,P+F(N)).

fib2(0)->0;
fib2(1)->1;
fib2(N) when N>1 -> fib2(N-2,0,1).
fib2(0,F1,F2)->F1+F2;
fib2(N,F1,F2)->fib2(N-1,F2,F1+F2).

perfect(N) -> perfect(N,N div 2,0).
perfect(N,0,A) -> N==A;
perfect(N,D,A) when N rem D == 0 -> perfect(N,D-1,A+D);
perfect(N,D,A) -> perfect(N,D-1,A).

fibP(0) -> {0,1};
fibP(N) ->
	{P,C} = fibP(N-1),
	{C,P+C}.
fib3(N) -> 
	{X,_} = fibP(N),
	X.
	

