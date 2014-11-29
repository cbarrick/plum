% map.pl
% An encoding of and utilities for the map in the board-game Clue.
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


:- module('map', [
	node/1,
	edge/2,
	path/3,
	distance_estimate/3,
	reachable/3
]).

:- use_module('cards').
:- use_module('heap').
:- use_module('util').


% Nodes
% --------------------------------------------------

%% node(?N)
% True when `N` is a valid node. Nodes take one of three forms:
% - `point(X, Y)` refers to a node outside of any room.
% - `room(Name, CardID)` refers to a room. This is the same form as the card
%    predicate for the room.
% - `suspect(Name, ID)` refers to the start space for a suspect. The
%   argument is the same form as the card predicate for the suspect.
% - `node(N)` where `N` is a node. This recursive definition is useful to deal
%   with nodes in general without needing a separate case for each form.

node(point(X, Y)) :-
	between(1, 24, X),
	between(1, 25, Y),
	\+ excluded(point(X,Y)).

node(room(Name, ID)) :-
	room(Name, ID).

node(suspect(Name, ID)) :-
	suspect(Name, ID).

node(node(N)) :- node(N).


%% excluded(?Point)
% Not all `point(X,Y)` terms are valid nodes. Firstly, the board is 24x25, so
% X must be between 1 and 24, and Y must be between 1 and 25. Secondly, the
% board is not perfectly square. And finally, nodes are identified unquely;
% point nodss that overlap start nodes or room nodes must be excluded. This
% predicate is true when `Point` is not a valid point node.

% The board is not perfectly square.
% We exclude some point nodes along the sides.
excluded( point(1,9)   ).
excluded( point(1,17)  ).
excluded( point(1,19)  ).
excluded( point(7,1)   ).
excluded( point(7,2)   ).
excluded( point(8,1)   ).
excluded( point(9,1)   ).
excluded( point(9,25)  ).
excluded( point(16,1)  ).
excluded( point(16,25) ).
excluded( point(17,1)  ).
excluded( point(18,1)  ).
excluded( point(18,2)  ).
excluded( point(24,8)  ).
excluded( point(24,14) ).
excluded( point(24,21) ).

% Start nodes are referred to as `start(suspect(Name, CardID))`.
% We exclude point nodes that overlap with start nodes so they cannot be
% referred to in `point(X,Y)` form.
excluded( point(1,18)  ). % mustard
excluded( point(8,25)  ). % scarlet
excluded( point(10,1)  ). % white
excluded( point(15,1)  ). % green
excluded( point(24,7)  ). % peacock
excluded( point(24,20) ). % plum

% Room nodes are referred to as `room(Name, CardID)`.
% We exclude point nodes that overlap with room nodes. We simplify this process
% by using a helper `excluded_range/3` that allows us to define a bounding box
% for multiple nodes to be excluded.
excluded(point(X,Y)) :-
	excluded_range(point(A,B), Width, Height),
	A_ is A + Width - 1,
	B_ is B + Height - 1,
	between(A, A_, X),
	between(B, B_, Y).

% In general, points must be within bounds.
excluded(point(X,_)) :- integer(X), X < 1, X > 24.
excluded(point(_,Y)) :- integer(Y), Y < 1, Y > 25.


%% excluded_range(?TopLeft, ?Width, ?Height)
% This auxillary predicate specifies room boundaries. Point nodes may not exist
% within these boundaries.

% kitchen
excluded_range(point(1,1), 6, 7).

% dining room
excluded_range(point(1,10), 5, 1).
excluded_range(point(1,11), 8, 6).

% lounge
excluded_range(point(1,20), 7, 6).

% ballroom
excluded_range(point(9,3), 2, 6).
excluded_range(point(11,1), 4, 8).
excluded_range(point(15,3), 2, 6).

% hall
excluded_range(point(10,19), 6, 7).

% conservatory
excluded_range(point(19,1), 1, 5).
excluded_range(point(20,1), 5, 6).

% billiard room
excluded_range(point(19,9), 6, 5).

% library
excluded_range(point(18,16), 1, 3).
excluded_range(point(19,15), 6, 5).

% study
excluded_range(point(18,22), 7, 4).

% "cellar" / empty space in the middle
excluded_range(point(11,11), 5, 7).


% Edges
% --------------------------------------------------

%% edge(?A, ?B)
% True when there is an edge between nodes `A` and `B`.

edge(node(A), B) :- nonvar(A), !, edge(A, B).
edge(A, node(B)) :- nonvar(B), !, edge(A, B).

edge(A, B) :- edge_(A, B), node(A), node(B).
edge(A, B) :- edge_(B, A), node(A), node(B).

% Start spaces
edge_( suspect(mustard,_),  point(2,18)  ).
edge_( suspect(scarlet,_),  point(8,24)  ).
edge_( suspect(white,_),    point(10,2)  ).
edge_( suspect(green,_),    point(15,2)  ).
edge_( suspect(peacock,_),  point(23,7)  ).
edge_( suspect(plum,_),     point(23,20) ).

% Secret Passages
edge_( room(kitchen,_),  room(study,_)        ).
edge_( room(lounge,_),   room(conservatory,_) ).

% Doors
edge_( room(kitchen,_),       point(5,8)   ).
edge_( room(dining,_),        point(9,13)  ).
edge_( room(dining,_),        point(7,17)  ).
edge_( room(lounge,_),        point(7,19)  ).
edge_( room(ballroom,_),      point(8,6)   ).
edge_( room(ballroom,_),      point(10,9)  ).
edge_( room(ballroom,_),      point(17,6)  ).
edge_( room(ballroom,_),      point(15,9)  ).
edge_( room(hall,_),          point(12,18) ).
edge_( room(hall,_),          point(13,18) ).
edge_( room(hall,_),          point(16,21) ).
edge_( room(conservatory,_),  point(19,6)  ).
edge_( room(billiard,_),      point(18,10) ).
edge_( room(billiard,_),      point(23,14) ).
edge_( room(library,_),       point(21,14) ).
edge_( room(library,_),       point(17,17) ).
edge_( room(study,_),         point(18,21) ).

% Other Points
edge_(point(X1,Y1), point(X2,Y2)) :- nonvar(X1), X2 is X1 + 1, Y1 = Y2.
edge_(point(X1,Y1), point(X2,Y2)) :- nonvar(X1), X2 is X1 - 1, Y1 = Y2.
edge_(point(X1,Y1), point(X2,Y2)) :- nonvar(Y1), Y2 is Y1 + 1, X1 = X2.
edge_(point(X1,Y1), point(X2,Y2)) :- nonvar(Y1), Y2 is Y1 - 1, X1 = X2.


% Path finding
% --------------------------------------------------

%% path(+Start, +Goal, -Path)
% Finds the shortest path between nodes Start and Goal.

path(Start, Goal, Path) :-
	node(Start),
	node(Goal),
	heap(EmptyQueue),
	EmptyQueue.init(util:ascending(map:a_star(Goal))),
	Queue = EmptyQueue.insert([Start]),
	best_first(Queue, Goal, Path).


%% best_first(+Queue, +Goal, -Path)
% This predicate implements best-first graph search. In practice, we always
% initialize the queue to use A* as the priority measure.

best_first(Queue, Goal, Path) :-
	Path = Queue.root,
	Path = [Goal|_],
	!.

best_first(Queue, Goal, Path) :-
	[H|CurrentPath] = Queue.root,
	findall([Next,H|CurrentPath], (
		edge(H, Next),
		\+ member(Next, [H|CurrentPath])
	), Successors),
	NextQueue = Queue.pop().insert_list(Successors),
	best_first(NextQueue, Goal, Path).


%% a_star(+GoalNode, +PartialPath, -AStarPriority)
% Calculates the A* value for a partial path, i.e. the total length of the
% partial path plus an estimate of the remaining distance to the goal.

a_star(Goal, [H|Path], Priority) :-
	length([H|Path], Distance),
	distance_estimate(H, Goal, Estimate),

	% Since entering a room consumes a turn, we penalize the priority by 6
	% distance units per room. I chose 6 as the penalty because it is the most
	% likely move distance per turn, and passing through a room costs 1 turn.
	findall(Room, member(room(Room,_), Path), Rooms),
	length(Rooms, NumberOfRooms),
	Penalty = 6 * NumberOfRooms,

	Priority is Distance + Estimate + Penalty.


%% distance_estimate(+A, +B, -Distance)
% Estimates the distance between two nodes.

distance_estimate(X, X, 0) :- !.
distance_estimate(X, Y, 1) :- edge(X, Y), !.

% The distance between two points,
% the manhattan distance formula:
distance_estimate(point(X1,Y1), point(X2,Y2), Dist) :- !,
	Dist is abs(X1-X2) + abs(Y1-Y2).

% The distance between a room/start and a point:
% Delegates to `distance_estimate_/3`.
distance_estimate(point(X,Y), Room, Dist) :- !, distance_estimate(Room, point(X,Y), Dist).
distance_estimate(Room, point(X,Y), Dist) :- !,
	findall(D, distance_estimate_(point(X,Y), Room, D), Ds),
	sort(Ds, [Dist|_]).

% The distance between two rooms/starts:
% We take the shortest distance among all estimates from B to the doors of A.
distance_estimate(A, B, Dist) :- !,
	findall(D, (
		edge(A, point(X1,Y1)),
		distance_estimate(point(X1,Y1), B, D0),
		D is D0 + 1 % Add 1 for entering the room.
	), Ds),
	sort(Ds, [Dist|_]).


%% distance_estimate_(+Point, +OtherNode, -Distance)
% Estimates the distance between a room/start node and a point node.

% Without secret passages, we just find the shortest distance among all point
% nodes adjacent to the room (i.e. the doors) to the goal point.
distance_estimate_(point(X,Y), Room, Dist) :-
	findall(D, (
		edge(Room, point(A,B)),
		distance_estimate(point(A,B), point(X,Y), D)
	), Ds),
	sort(Ds, [ShortestDist|_]),
	Dist is 1 + ShortestDist. % Add 1 for entering the room.

% With secret passages, we also consider the distance among all point nodes
% adjacent to the other room. We add 6 to this distance since taking a secret
% passage consumes a turn, and 6 is the most likely move distance for a turn.
distance_estimate_(point(X,Y), Room, Dist) :-
	Other = room(_,_),
	edge(Room, Other),
	once(distance_estimate_(Other, point(X,Y), D0)),
	Dist is D0 + 1 + 6. % Add 1 for entering the room.


% Miscelaneous
% --------------------------------------------------

%% reachable(?Node, +From, +Steps)
% Node is reachable from the other node within the given number of steps
% assuming a single turn (i.e. you enter then exit a room). This predicate
% eagerly finds all solutions first then backtracks over them.

reachable(Node, From, Steps) :-
	Steps > 0,
	Steps0 is Steps - 1,
	setof(N, Start^Nodes^(
		edge(From, Start),
		reachable_(Steps0, [Start], Nodes),
		member(N, Nodes)
	), Ns),
	!,
	member(Node, Ns).

reachable_(0, Seen, Seen) :- !.
reachable_(N, Seen, Nodes) :-
	findall(Successor, (
		member(point(X,Y), Seen),
		edge(point(X,Y), Successor),
		\+ member(Successor, Seen)
	), Successors),
	append(Seen, Successors, NextSeen),
	NextN is N - 1,
	reachable_(NextN, NextSeen, Nodes).
