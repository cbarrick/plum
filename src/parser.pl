% parser.pl
% A parser for the input language to the Plum agent.
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


:- module('parser', [
	parse/3
]).

:- use_module(library('dcg/basics')).

:- use_module('cards').
:- use_module('map').


%% parse(+Agent, +InputString, -ParseTree)
% Parses the input string as a statement to be interpreted by an Agent.

parse(Agent, InputString, ParseTree) :-
	phrase(statement(Agent, ParseTree), InputString, []).


%% statement(+Agent, ?Statement)//
% Parses a statement. A statement is the root of the syntax tree interpreted by
% the agent. It is a compound term of the form
%     `statement(Subject, Verb, Object, Modifiers)`
% where Subject and Object are nouns, Verb is a verb, and Modifiers is a list
% of prepositional phrases. Each part of speach may be represented by a noun or
% a compound term.

% Transitive verb
statement(Agent, statement(S,V,O,M) ) -->
	blanks,
	subject(Agent, Conj, S),
	verb(Conj, V),
	noun(Agent, O),
	modifiers(Agent, M),
	".",
	blanks.

% Intransitive verb
statement(Agent, statement(S,V,'',M) ) -->
	blanks,
	subject(Agent, Conj, S),
	verb(Conj, V),
	modifiers(Agent, M),
	".",
	blanks.


%% subject(+Agent, ?Conjugation, ?Subject)//
% Consumes a noun when the sentence is declarative and nothing at all when the
% sentence is imperative.
%
% The only nouns allowed to be subjects are players. If a noun is not given,
% the sentence is assumed to be an imperative (i.e. second conjugation), and the
% agent itself is taken to be the subject.
%
% - Conjugation is 2 if the subject is the agent, otherwise Conjugation is 3.
% - Subject is a member of `Agent.players`.

% Declarative case
subject(Agent, Conj, Sub) -->
	player(Agent, Sub),
	{
		(Sub = Agent.self ->
			Conj = 2
		;
			Conj = 3
		)
	}.

% Imperative case
subject(Agent, 2, Sub) --> {Sub = Agent.self}.


%% verb(?Conjugation, ?Verb)//
% Consumes a verb. Conjugation is either 2 or 3.

verb(2, has) --> word(have).
verb(3, has) --> word(has).

verb(2, moves) --> word(move).
verb(3, moves) --> word(moves).

verb(2, passes) --> word(pass).
verb(3, passes) --> word(passes).

verb(2, shows) --> word(show).
verb(3, shows) --> word(shows).

verb(2, suggests) --> word(suggest).
verb(3, suggests) --> word(suggests).

verb(2, debugs) --> word(debug).
verb(3, debugs) --> word(debugs).

verb(2, quits) --> word(quit).
verb(3, quits) --> word(quits).

verb(2, refers) --> word(refer).
verb(3, refers) --> word(refers).


%% noun(+Agent, ?Noun)//
% Consumes a noun, optionally preceeded by a determiner.
% - If the noun refers to a player, Noun is the coresponding member
%   of `Agent.players`.
% - If the noun refers to a card, Noun is a compound term coresponding to a
%   valid call to `cards:suspect/2`, `cards:weapon/2`, or `cards:room/2`.
% - If the noun refers to a node on the map, Noun is a compound term
%   coresponding to a valid call to `map:node/1`. Note that room names and
%   suspect names will be interpreted as cards, not nodes. This effectivly
%   limits this type of noun to point nodes.
% - Integers are considered nouns.
% - All other words that are not know verbs are considered nouns. In this case,
%   Noun is the word as an atom.

% All objects may be preceeded by a determiner
noun(Agent, O) -->
	determiner,
	noun(Agent, O).

% Known objects get parsed into compound terms
noun(Agent, Player) --> player(Agent, Player).
noun(_, Suspect)    --> suspect(Suspect).
noun(_, Weapon)     --> weapon(Weapon).
noun(_, Room)       --> room(Room).
noun(_, Node)       --> node(Node).

% Distances will be parsed into integers
noun(_, Distance) -->
	integer(Distance),
	blanks.

% Other nouns will be parsed into atoms
noun(_, O) --> (\+ verb(_,_)), word(O).


%% player(+Agent, ?Player)//
% Consumes a reference to a player. References may be strings of the form "pN"
% where N is the index of that player (e.g. "p1" for player one). Players can
% also be refered to by name if the Agent knows their name. The strings "you"
% and "You" are interpreted as refering to the Agent itself.

player(Agent, Player) -->
	(word('you') | word('You')),
	{
		Player = Agent.self
	}.

player(Agent, Player) -->
	"p",
	integer(ID),
	blanks,
	{
		Player = Agent.select_player(ID,_)
	}.

player(Agent, Player) -->
	word(Name),
	{
		Player = Agent.select_player(Name,_)
	}.


%% room(?Room)//
% Consumes a reference to a room. Room is a term `room(Name,ID)` coresponding to
% a valid call to `cards:room/2`. The string consumed is the name of the room
% optionally followed by the word "room".

room( room(Name,ID) ) -->
	word(Name),
	(word(room) | []),
	{
		room(Name, ID)
	}.


%% suspect(?Suspect)//
% Consumes a reference to a suspect. Suspect is a term `suspect(Name,ID)`
% coresponding to a valid call to `cards:suspect/2`. The string consumes is the
% name of the suspect optionally prefixed with an honorific. The honorific must
% not end with a period.

suspect( suspect(Name,ID) ) -->
	(word(mr) | word(mrs) | word(miss) | word(coln) | word(prof) | []),
	word(Name),
	{
		suspect(Name, ID)
	}.


%% weapon(?Weapon)//
% Consumes a reference to a weapon. Weapon is a term `weapon(Name,ID)`
% coresponding to a valid call to `cards:weapon/2`. The string consumed is the
% name of the weapon. In the case of the pipe, the adjective "lead" may preceed
% the name.

weapon( weapon(Name,ID) ) -->
	word(Name),
	{
		weapon(Name, ID)
	}.

weapon( weapon(pipe,ID) ) -->
	word(lead),
	word(pipe),
	{
		weapon(pipe, ID)
	}.


%% node(?Point)//
% Consumes a reference to a point node on the map. Point is a term
% `node(point(X,Y))` coresponding to a valid call to `map:node/1`. The string
% consumed is the ordered pair "(X,Y)" where X and Y are integers.

node( node(point(X,Y)) ) -->
	"(",
	blanks,
	integer(X),
	blanks,
	",",
	blanks,
	integer(Y),
	blanks,
	")",
	blanks,
	{
		node(point(X,Y))
	}.


%% modifiers(+Agent, ?Ms)//
%% modifier(+Agent, ?M)//
% Consumes a prepositional phrase or a list of prepositional phrases. Ms is the
% sorted list of phrases. Each phrase is a compound term whose functor is the
% preposition used and whose only parameter is the object of the preposition.
% The prepositions understood by the parser are "in", "with", "of", "from",
% "as", and "to".

modifiers(Agent, Ms) -->
	modifier(Agent, H),
	modifiers(Agent, T),
	{
		sort([H|T], Ms)
	}.
modifiers(_, []) --> [].

modifier(Agent, M) -->
	word(Preposition),
	noun(Agent, Object),
	{
		member(Preposition, [in, with, of, from, as, to]),
		functor(M, Preposition, 1),
		arg(1, M, Object)
	}.


%% determiner//
% Nouns may have determiners for syntactic sugar.

determiner --> word(the).
determiner --> word(a).
determiner --> word(an).


%% word(-W)//
% Consumes any word. `W` is the word as an atom.

word(W) -->
	string_without(". ", Codes),
	blanks,
	{
		length(Codes, L),
		L >= 1,
		atom_codes(W, Codes)
	}.
