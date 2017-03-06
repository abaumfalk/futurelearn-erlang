-module(index).
-export([preprocess/1,get_file_contents/1,show_file_contents/1,index/1,word/1,words/1,remove_short/2,index_add_word/3,index_add_words/3,cook_lines/1,cook_index/1]).

%%%%%%%%%%%%%%%%%%
% file operations
%%%%%%%%%%%%%%%%%%
% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
show_file_contents([]) ->
    ok.    
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Indexing is done in following steps:
% 1. read the file
% 2. preprocessing
%    - filter punctuation
%    - capitalize
%    - cut into words
%    - filter out small words
% 3. build "raw" index of {word, [line1,line2,...]} tuples
% 4. postprocessing
%    - calculate ranges from the recorded lines ("raw" -> "cooked")
%	 - sort the index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index(Name) ->
	File = get_file_contents(Name),
	Fs = preprocess(File),
	Is = index(Fs, [], 1),
	postprocess(Is).

%%%%%%%%%%%%%%%%
% preprocessing	
%%%%%%%%%%%%%%%%
preprocess(File) ->
	Filt = capitalize(filter_punctuation(File)),
	filter_small(towords(Filt), 3).

% capitalize file
capitalize([]) -> [];
capitalize([L|Ls]) ->
	[toupper(L) | capitalize(Ls)].
	
% filter out special chars
filter_punctuation([]) -> [];
filter_punctuation([L|Ls])	 ->
	[trim(L) | filter_punctuation(Ls)].

% convert to words
towords([]) ->  [];
towords([L|Ls]) ->
	[words(L) | towords(Ls)].

% filter out small words
filter_small([], _) -> [];
filter_small([L|Ls], N) ->
	[remove_short(L, N) | filter_small(Ls, N)].

%%%%%%%%%%%
% indexing
%%%%%%%%%%%
% create a raw index Is of a list of lines, which contains a list of words
% the raw format is a tuple {Word, ListOfLines}
% e.g. [["THIS", "IS", "FIRST"], ["THIS", ...],...] -> [{"THIS", [2,1]},...]
index([], Is, _) -> 
	Is; 
index([Line|Lines], Is, N) ->
	index(Lines, index_add_words(Line, Is, N), N+1).

% add a list of words to an existing raw index Is
index_add_words([], Is, _N) -> 
	Is;
index_add_words([W|Ws], Is, N) -> 
	Word = toupper(W),
	index_add_words(Ws, index_add_word(Word, Is, N), N).

%add a single word to an existing raw index Is
index_add_word([], Is, _) -> 
	Is;
index_add_word(W, [], N) ->
	[{W, [N]}];
index_add_word(W, [{Wi, Ls}|Is], N) ->
	case W == Wi of
	true ->
		% the word is already in the index, so add the current line number
		[{Wi, [N|Ls]} | Is];
	false ->
		% the word is not in the index, so add it
		[{Wi, Ls} | index_add_word(W, Is, N)]
	end.
	
%%%%%%%%%%%%%%%%%
% postprocessing
%%%%%%%%%%%%%%%%%
postprocess(Is) ->
	qsort(cook_index(Is)).

% convert the index from "raw" to "cooked" format as given in the spec
cook_index([]) -> 
	[];
cook_index([{W,Ls} | Is]) ->
	[{W,cook_lines(Ls)} | cook_index(Is)].

% convert a list of raw line numbers Lr into a list of 
% cooked line ranges Lc as given in the spec
% we can assume that we get the raw lines backwards-sorted here
% e.g.: [13,12,11,7,5,4,3] -> [{3,5},{7,7},{11,13}]
cook_lines([]) -> 
	[];
cook_lines(Lr) ->
	cook_lines(Lr, []).
cook_lines([], Lc) ->
	Lc;
cook_lines([Lr|Lrs], []) ->	
	cook_lines(Lrs, [{Lr,Lr}]);
cook_lines([Lr|Lrs], [{Ls,Le} | Lcs]) ->
	case (Lr == Ls - 1) orelse (Lr == Ls) of
	true ->
		cook_lines(Lrs, [{Lr,Le} | Lcs]);
	false ->
		cook_lines(Lrs, [{Lr,Lr} | [{Ls,Le} | Lcs]])
	end.
	
%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%

% join two lists together keeping the original order
join([], Ys) -> Ys;
join([X|Xs],Ys) ->
	[X | join(Xs,Ys)].

% remove all special chars from list keeping only letters and spaces
% e.g.: "I'm a full sentence." -> "Im a full sentence"
trim([]) -> [];
trim([X|Xs]) ->
	case (X >= $a andalso X =< $z) orelse (X >= $A andalso X =< $Z) orelse (X == 32) of 
		true -> 
			[X | trim(Xs)];
		false ->
			trim(Xs)
	end.

%
% extract words from a line of text, omitting empty words
% e.g.: "This is a  line  of text." -> ["This", "is", "a", "line", "of", "text."]
words([]) -> [];
words(Ls) ->
	{Word, Rest} = word(Ls),
	% filter empty words (due to lines which have only separators)
	case Word == [] of
	true ->
		words(Rest);
	false ->
		[Word | words(Rest)]
	end.
	
% get first word of line and rest of line as tuple (using space char 
% as separator)
% e.g.: "Line of text." -> {"Line", "of text."}
word([]) -> {[], []};
word(Ls) ->
	word(Ls, []).
word([], Word) ->
	{lists:reverse(Word), []};
word([L|Ls], Word) ->
	case L == 32 of 
	true ->
		% filter empty words (due to multiple separators)
		case Word == [] of
		true ->
			word(Ls, []);
		false ->
			{lists:reverse(Word), Ls}
		end;
	false ->
		word(Ls, [L|Word])
	end.
	
%
% remove words shorter than N letters from a list of words
% e.g.: "This is a line", 3 -> "This line"
remove_short([], _N) -> [];
remove_short([L|Ls], N) ->
	case length(L) < N of
	true ->
		remove_short(Ls, N);
	false ->
		[L | remove_short(Ls, N)]
	end.

%
% convert word to uppercase to normalize capitalization
% e.g.: "hello" -> "HELLO"
toupper([]) -> [];
toupper([X|Xs]) ->
	case X >= $a andalso X =< $z of
		true ->
			[ X -32 | toupper(Xs) ];
		false ->
			[ X | toupper(Xs) ]
	end.

%%%%%%%%%%%%%
%% quicksort
%%%%%%%%%%%%%
qsort([]) -> [];
qsort([X]) -> [X];
qsort([X|Xs]) ->
	{Ys, Zs} = partition(X, [X|Xs]),
	join(qsort(Ys), qsort(Zs)).

% partition a list using given pivot P into two lists {L1, L2}
% where all elements =<P are in L1, all others in L2
partition(_P, []) -> [];
partition(P, Xs) ->
	partition(P, Xs, [], []).
partition(_P, [], Ys, Zs) ->
	{Ys, Zs};
partition(P, [X|Xs], Ys, Zs) ->
	case X>P of
		true ->
			partition(P, Xs, Ys, [X|Zs]);
		false ->
			partition(P, Xs, [X|Ys], Zs)
	end.
