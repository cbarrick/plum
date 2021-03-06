% cards.pl
% An encoding of the cards in the board-game Clue.
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


:- module('cards', [
	card/2,
	suspect/2,
	weapon/2,
	room/2,
	print_card/1
]).

:- use_module(library('ansi_term')).


%% card(?Card, ?ID)
% True when Card is any card in the game and ID is its integer identifier.
% Card may take one of three forms:
% - An atom naming the card
% - A term `suspect(Name,ID)`, `weapon(Name,ID)`, or `room(Name,ID)` refering to
%   the type of the card.
% - A term `card(C,ID)` where C is a card. This recursive definition is useful
%   to deal with cards in general without knowing their type.

card( card(C,ID), ID )       :- nonvar(C), !, card(C, ID).
card( suspect(Name,ID), ID ) :- nonvar(Name), !, suspect(Name, ID).
card( weapon(Name,ID),  ID ) :- nonvar(Name), !, weapon(Name, ID).
card( room(Name,ID),    ID ) :- nonvar(Name), !, room(Name, ID).
card( Name, ID ) :- suspect(Name, ID).
card( Name, ID ) :- weapon(Name, ID).
card( Name, ID ) :- room(Name, ID).


%% suspect(?Name, ?ID)
% Name is an atom naming a suspect card in the game and ID is the unique integer
% identifier for that card.

suspect(green,      1).
suspect(mustard,    2).
suspect(peacock,    3).
suspect(plum,       4).
suspect(scarlet,    5).
suspect(white,      6).


%% weapon(?Name, ?ID)
% Name is an atom naming a weapon card in the game and ID is the unique integer
% identifier for that card.

weapon(candlestick, 7).
weapon(knife,       8).
weapon(pipe,        9).
weapon(revolver,   10).
weapon(rope,       11).
weapon(wrench,     12).


%% room(?Name, ?ID)
% Name is an atom naming a room card in the game and ID is the unique integer
% identifier for that card.

room(ballroom,     13).
room(billiard,     14).
room(conservatory, 15).
room(dining,       16).
room(hall,         17).
room(kitchen,      18).
room(library,      19).
room(lounge,       20).
room(study,        21).


%% print_card(+Card)
% Prints the name of the card to the terminal with fancy formatting.

% The card can be an atom, an ID, or a compound.
% These first two cases normalize the card into an atom
print_card(ID) :-
	integer(ID),
	card(X, ID),
	!,
	print_card(X).
print_card(X) :-
	compound(X),
	card(X,ID),
	card(Name, ID),
	!,
	print_card(Name).

% The rest of the cases do the real work
print_card(Name) :-
	atom(Name),
	suspect(Name,_),
	!,
	member([Honorific,Name], [
		[miss, scarlet],
		[prof, plum],
		[mrs, peacock],
		[mr, green],
		[coln, mustard],
		[mrs, white]
	]),
	format("~w ", [Honorific]),
	ansi_format([bold,underline], "~w", [Name]).
print_card(Name) :-
	atom(Name),
	weapon(Name,_),
	!,
	write("the "),
	(Name = pipe ->
		ansi_format([bold,underline], "lead ", [])
	; true),
	ansi_format([bold,underline], "~w", [Name]).
print_card(Name) :-
	atom(Name),
	room(Name,_),
	!,
	write("the "),
	ansi_format([bold,underline], "~w", [Name]),
	(member(Name, [billiard, dining]) ->
		write(" room")
	; true).
