-module(second).
-export([hyp/2,perimeter/2,area/2,howManyEqual/3,maxThree/3]).

hyp(A,B) ->
	S = first:square(A)+first:square(B),
	math:sqrt(S).

perimeter(A,B) ->
	C = hyp(A,B),
	A+B+C.

area(A,B) ->
	C = hyp(A,B),
	first:area(A,B,C).

maxThree(X,Y,Z) ->
	max(max(X,Y),Z).	

howManyEqual(X,X,X) ->
	3;
howManyEqual(X,X,_) ->
	2;
howManyEqual(X,_,X) ->
	2;
howManyEqual(_,X,X) ->
	2;
howManyEqual(_,_,_) ->
	0.
