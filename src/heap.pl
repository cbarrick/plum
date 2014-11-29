% heap.pl
% A priority queue implementation for SWI Prolog 7
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


:- module('heap', [
	heap/1
]).


% This module implements a priority queue via pairing heap. A pairing heap
% is a simplified fibonacci heap. They offer constant time pop, peek, and merge
% as well as logarithmic time insert. Pairing heaps are generally better than
% binary and binomial heaps. See [Wikipedia] for details.
%
% The interface makes use of the object-oriented [dictionary syntax] in
% SWI-Prolog 7. Thus this module is likely not portable.
%
% [Wikipedia]: http://en.wikipedia.org/wiki/Pairing_heap
% [dictionary syntax]: http://www.swi-prolog.org/pldoc/man?section=dicts


%% heap(?H)
% True when H is a heap as implemented by this module.

heap(heap{
	comparator: _,
	empty: _,
	root: _,
	children: _
}).


%% +Heap.init(+Comparator) := true
% Initialize a heap with a Comparator. A comparator is a term that will be
% called with 3 additional arguments. The first two are the items being compared
% and the third should become unified with the item which should come first.

:- meta_predicate(init(3, +, -)).

Heap.init(Comparator) := true :-
	heap(Heap),
	Heap.comparator = Comparator,
	Heap.empty = true,
	Heap.children = [].


% Peek
% --------------------------------------------------
% Peeking the root element is trivial.

%% +Heap.peek() := -Element
% Element is the root of the heap.

Heap.peek() := Heap.root :- heap(Heap), \+ Heap.empty.


% Insert
% --------------------------------------------------
% Inserting into a pairing heap is trivial. We create a new heap containing only
% the element to be inserted and merge it into the original heap.

%% +Heap.insert(+Elm) := -NewHeap
% NewHeap contains the elements of Heap and the additional element Elm.

Heap.insert(Elm) := NewHeap :-
	heap(Heap),
	heap(EmptyHeap),
	EmptyHeap.init(Heap.comparator),
	SingleHeap = EmptyHeap.put(heap{
		empty: false,
		root: Elm
	}),
	NewHeap = Heap.merge(SingleHeap).


% Insert List
% --------------------------------------------------

%% +Heap.insert_list(+List) := -NewHeap
% NewHeap contains all the elements of Heap and all the elements of List.

Heap.insert_list([]) := Heap :- heap(Heap), !.

Heap.insert_list([H|T]) := NewHeap :-
	heap(Heap),
	TmpHeap = Heap.insert(H),
	NewHeap = TmpHeap.insert_list(T).


% Merge
% --------------------------------------------------
% Pairing heaps are easy to merge. Each `heap{...}` has a root key and a list
% of child heaps. The key of each child satisfies the heap property relative to
% the parent. To merge two heaps, we figure out which should be the parent and
% which should be the child by comparing their root keys and then append the
% child to the parent's list of children.

%% Heap.merge(+Other) := -NewHeap
% NewHeap is the merger of `Heap` and `Other`. Both heaps must have the same
% comparator.

% Throw an error if the heaps cannot be merged
Heap.merge(Other) := false :-
	heap(Heap),
	Heap.comparator \= Other.comparator,
	!,
	swritef(ErrorMessage,
	        "Cannot merge heap of type %w with heap of type %w",
	        [Heap.comparator, Other.comparator]),
	Error = error(invalid_merge(Heap.comparator, Other.comparator),
	              context(merge/3, ErrorMessage)),
	throw(Error).

% Merging is commutative. We delegate to a helper to reduce code duplication.
Heap.merge(Other) := NewHeap :- merge_(Heap, Other, NewHeap), !.
Heap.merge(Other) := NewHeap :- merge_(Other, Heap, NewHeap).


merge_(Heap, Other, NewHeap) :-
	heap(Heap), heap(Other),
	Other.empty,
	NewHeap = Heap,
	!.
merge_(Heap, Other, NewHeap) :-
	heap(Heap), heap(Other),
	\+ Heap.empty,
	member(Parent, [Heap, Other]),
	member(Child, [Heap, Other]),
	Parent \= Child,
	call(Heap.comparator, Heap.root, Other.root, Parent.root),
	NewHeap = Parent.put(children, [Child|Parent.children]).


% Pop
% --------------------------------------------------
% The only non-trivial operation is the deletion of the root element from the
% heap. The standard strategy first merges the subheaps in pairs (this is the
% step that gave this datastructure its name) from left to right and then
% merges the resulting list of heaps from right to left.

%% +Heap.pop() := -NewHeap
% NewHeap contains all the element of Heap except the root element of Heap.

Heap.pop() := NewHeap :-
	heap(Heap),
	\+ Heap.empty,
	length(Heap.children, L), L > 0,
	merge_pairs(Heap.children, NewHeap),
	!.

Heap.pop() := NewHeap :-
	heap(Heap),
	\+ Heap.empty,
	length(Heap.children, L), L = 0,
	heap(NewHeap),
	NewHeap.init(Heap.comparator).


merge_pairs([X],           X) :- !.
merge_pairs([A,B],   NewHeap) :- NewHeap = A.merge(B), !.
merge_pairs([A,B|T], NewHeap) :-
	C = A.merge(B),
	merge_pairs(T, D),
	NewHeap = C.merge(D).


% Size
% --------------------------------------------------
%

%% +Heap.size() := -Size
% Size is the number of elements in the heap.

Heap.size() := Size :-
	heap(Heap),
	Heap.empty,
	Size = 0,
	!.

Heap.size() := Size :-
	heap(Heap),
	size_(Heap.children, S0),
	Size is 1 + S0.


% Calculates the combined size of a list of heaps.
% Size is an arithmetic term, NOT A NUMBER, to allow for tail recursion.
size_([], 0).
size_([H|T], Size) :-
	S1 = H.size(),
	Size = S0 + S1,
	size_(T, S0).


% Print
% -------------------------------------------------------
%

Heap.print() := true :- !,
	heap(Heap),
	write('<'),

	( Heap.empty, !
	; write(Heap.root)
	),

	( Heap.size() =< 1, !
	; forall(member(Child, Heap.children), (
		write(','),
		Child.print()
	  ))
	),

	write('>').
