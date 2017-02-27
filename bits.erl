-module(bits).
-export([bits/1,bits1/1,bits2/1,bits3/1,bits4/1,test/0]).


%%in this module we define several functions to calculate the number
%%of bits set to '1' in an arbitrary positive integer

%%this version uses log2, which gives back a float, so the calculation
%%is limited to numbers that fit into floats - it will give false 
%%results otherwise!
bits1(N) -> bits1(N,0).
bits1(N,S) when N == 0 -> S;
bits1(N,S) -> 
	MSB = trunc(math:log2(N)),
	bits1(N - math:pow(2,MSB), S+1).
	
%%here we use an accumulator to sum up the number of bits
bits2(N) -> bits2(N,0).
bits2(0,S) -> S;
bits2(N,S) when N rem 2 == 0 -> bits2(N div 2, S);
bits2(N,S) -> bits2(N div 2, S + 1).
	
%%in this version the accumulator is not needed anymore
bits3(0) -> 0;
bits3(N) when N rem 2 == 0 -> bits3(N div 2);
bits3(N) -> 1 + bits3(N div 2).
	
%%version without tail recursion
bits4(0) -> 0;
bits4(N) -> 
	S = bits4(N div 2),
	S + N rem 2.

%%we define bits3 as default, for it seems the most efficient version
bits(N) -> bits3(N).

%%we test the bits function checking a list of predefined results
test() -> test([{8,1},{7,3},{255,8},{256,1}]).
test([]) -> test_passed;
test([E|Tail]) ->
	{Argument, Expected} = E,
	Result = bits(Argument),
	Result = Expected, %%this will raise an exception if Result =!= Expected
	test(Tail).
