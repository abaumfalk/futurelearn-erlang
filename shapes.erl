-module(shapes).
-export([perimeter/1,area/1,enclose/1]).
 
%%This module defines a few functions to calculate properties 
%%of some shapes. 
%%The definition of the shapes omits the location and orientation, for
%%we are interested in the pure shape only.
%%
%%Following shapes are supported:
%%rectangle with height H, width W:
%%{rectangle, H, W}
%%
%%circle with radius R:
%%{circle, R}
%%
%%triangle with sides of length A, B and C 
%%(in order to get a vaild triangle we have to ensure that the length of
%%any side is smaller than the sum of the length of the other sides)
%%{triangle, A, B, C} 

%%calculate the perimeter of a shape
perimeter({rectangle, H, W}) ->
	2*H + 2*W;
perimeter({circle, R}) ->
	2*math:pi()*R;
perimeter({triangle, A, B, C}) ->
	A + B + C.

%%calculate the area of a shape
area({rectangle, H, W}) ->
	H*W;
area({circle, R}) ->
	2*math:pi()*R;
area({triangle, A, B, C}) ->
	P = (A + B + C) / 2,
	math:sqrt(P*(P-A)*(P-B)*(P-C)).

%%calculate the smallest enclosing rectangle of a shape
enclose({rectangle, H, W}) ->
	{rectangle, H, W};
enclose({circle, R}) ->
	{rectangle, 2*R, 2*R};
%%for the triangle we arbitrarily choose a rectangle with a width of 
%%side C - choosing a different side will result in a rectangle with 
%%the same area.
enclose({triangle, A, B, C}) when A<B+C, B<A+C, C<A+B ->
	W = C,
	H = 2.0 * area({triangle, A, B, C}) / C,
	{rectangle, H, W}.
