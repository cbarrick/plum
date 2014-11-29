% util.pl
% Miscellaneous utility predicates
%
% Copyright (c) 2014 Chris Barrick
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.


:- module('util', [
	% Comparators
	ascending/3,
	ascending/4,
	descending/3,
	descending/4,

	% List predicates
	counts/2,
	partition/3,
	rotate/3,

	% CLP(FD) constraints
	empty_intersection/2,
	nonempty_intersection/2,
	reif_empty_intersection/3,
	reif_member/3
]).

:- use_module(library('clpfd')).


% Comparators
% --------------------------------------------------
% Comparators get passed around as terms that will be called with 3 additional
% arguments. The first two are the items being compared and the third should
% become unified with the item which should come first.

:- meta_predicate(ascending(2, +, +, -)).
:- meta_predicate(descending(2, +, +, -)).

%% ascending(+A, +B, -C), ascending(+Evaluator, +A, +B, -C)
%% descending(+A, +B, -C), descending(+Evaluator, +A, +B, -C)
% Given an Evaluator predicate, bind C to either A or B. The Evaluator is
% called with two additional arguments; the first is the item being evaluated,
% and the second is a variable which should become bound to a numeric value
% for the item. `ascending` binds C to the item with the smallest value, and
% `descending` binds C to the item with the largest value. If no evaluator is
% given, the arguments are compared directly.

% Note: We use `msort/2` below to sort on Prolog's natural ordering.
% `msort/2` is a builtin predicate in SWI that is like `sort/2` but maintains
% duplicates. See http://www.swi-prolog.org/pldoc/man?section=builtinlist

ascending(A, B, C) :-
	msort([A, B], [C, _]).

ascending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	ascending(ValueA, ValueB, ValueC),
	(
		ValueC = ValueA ->
		C = A
	;
		ValueC = ValueB,
		C = B
	),
	!.


descending(A, B, C) :-
	msort([A, B], [_, C]).

descending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	descending(ValueA, ValueB, ValueC),
	(
		ValueC = ValueA ->
		C = A
	;
		ValueC = ValueB,
		C = B
	),
	!.


% List Predicates
% --------------------------------------------------

%% counts(+In, -Out)
% In is a list of items and Out is a list of `[Count,Item]` pairs where Count is
% the number of times that element was seen in the input list. Out is sorted by
% descending count.

counts(In, Out) :-
	counts_(In, [], Out),
	!.

counts_([], Seen, Out) :-
	sort(Seen, Sorted),
	findall([Item,Count], member([Count,Item], Sorted), Backwards),
	reverse(Backwards, Out).
counts_([H|T], Seen, Out) :-
	select([Count,H], Seen, Rest),
	NextCount is Count + 1,
	counts_(T, [[NextCount,H]|Rest], Out).
counts_([H|T], Seen, Out) :-
	\+ member([_,H], Seen),
	counts_(T, [[1,H]|Seen], Out).


%% partition(+N, ?List, ?Parts)
% Partitions a list into N parts as evenly as possible. If List can't be
% partitioned evenly, the first few parts will have one more item than the last
% few parts.

partition(0, [], []).
partition(N, List, Parts) :-
	N > 0,
	length(Parts, N),
	length(List, L),
	PartSize is ceil(L/N),
	length(Part, PartSize),
	partition_(Part, List, ListRest),
	select(Part, Parts, PartsRest),
	N0 is N - 1,
	partition(N0, ListRest, PartsRest).

partition_([], Rest, Rest).
partition_([H|T], List, Rest) :-
	select(H, List, List_),
	partition_(T, List_, Rest).


%% rotate(+N, +ListIn, -ListOut)
% Rotates elements in a list by N steps.

rotate(0, List, List) :- !.
rotate(N, [H|T], Out) :-
	integer(N),
	N > 0,
	append(T, [H], Next),
	N0 is N - 1,
	!,
	rotate(N0, Next, Out).


% CLP(FD) constraints
% --------------------------------------------------

%% empty_intersection(+A, +B)
% The intersection of the lists of CLP(FD) variables, A and B, is empty.

empty_intersection(A, B) :-
	reif_empty_intersection(1, A, B).


%% nonempty_intersection(+A, +B)
% The intersection of the lists of CLP(FD) variables, A and B, is non-empty.

nonempty_intersection(A, B) :-
	reif_empty_intersection(0, A, B).


%% reif_empty_intersection(?Empty, +A, +B)
% A reifable version of empty_intersection/2. The intersection of the lists of
% CLP(FD), variables A and B, is empty iff the CLP(FD) variable Empty is 1.
% Otherwise, Empty is 0.

reif_empty_intersection(1, [], _) :- !.
reif_empty_intersection(1, _, []) :- !.
reif_empty_intersection(Empty, [H|A], B) :-
	reif_member(BHasHead, H, B),
	Empty #<==> (RestEmpty #/\ (#\ BHasHead)),
	reif_empty_intersection(RestEmpty, A, B).



%% reif_member(?Reif, +Element, +List)
% A reifable version of member/2 for a list of CLP(FD) variables. Reifability
% means the truth value of the statement is represented by a CLP variable.
% Reif is 1 when Element is a member of List and 0 when it is not.

reif_member(0, _, []).
reif_member(Reif, Element, [H|T]) :-
	Reif #<==> ((Element #= H) #\/ NextReif),
	reif_member(NextReif, Element, T).
