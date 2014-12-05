% agent.pl
% An agent that plays the board-game Clue.
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


:- module('agent', [
	agent/1
]).

:- use_module(library('ansi_term')).
:- use_module(library('clpfd')).
:- use_module(library('readutil')).

:- use_module('cards').
:- use_module('map').
:- use_module('parser').
:- use_module('util').


% Agent Constructor
% ==================================================

%% agent(?A)
% A is an agent.

agent(agent{
	self: _,
	cards: _,
	case: _,
	players: _,
	player_count: _,
	order: _,

	full_search: _,
	cache: _,
	current_suggestion: _,
	shown_counts: _
}).


%% +Agent.initialize(+PlayerCount, +Order) := true
% Initializes an agent. PlayerCount is the number of players and Order is the
% play order of the agent, e.g. id Order is 3, the agent is the third player.

Agent.initialize(N, Order) := true :-
	% Sanity check
	agent(Agent),
	between(3, 6, N),
	between(1, N, Order),

	% Create the case file from one suspect, one weapon, and one room
	% The goal of the agent is to discover these cards
	Killer in 1..6,
	Weapon in 7..12,
	Room in 13..21,
	Case = [Killer, Weapon, Room],

	% Create a list of players
	% Each player has a turn order and a hand of cards
	length(AllCardsInHands, 18), % 21 cards minus 3 in the case
	partition(N, AllCardsInHands, Hands),
	make_players(Hands, Players),

	% Collect all the cards used in players' hands and the case file
	% All cards must be unique from 1 to 21
	append([Case, AllCardsInHands], AllCards),
	length(AllCards, 21),
	AllCards ins 1..21,
	all_distinct(AllCards),

	% Find the player that represents the agent itself
	nth1(Order, Players, Self),

	Agent.case = Case,
	Agent.cards = AllCards,
	Agent.players = Players,
	Agent.player_count = N,
	Agent.self = Self,
	Agent.order = Order,
	Agent.full_search = false,

	length(Agent.self.hand, NumberOfCards),
	length(Agent.shown_counts, NumberOfCards),
	Agent.shown_counts ins 0, % Unifies all members to 0
	!.


%% player(?P)
% True when P is a player. Players are objects refering to players of the game.

player(player{
	id: _,
	name: _,
	hand: _
}).


%% make_players
% A helper predicate to construct players given lists of hands.

make_players(Hands, Players) :-
	make_players_(Hands, Players, 1).

make_players_([], [], _).
make_players_([H|Hands], [P|Players], ID) :-
	player(P),
	P = player{
		id: ID,
		name: Name,
		hand: H
	},
	chain(H, #<),
	ID1 is ID + 1,
	atom_concat(p, ID, Name),
	make_players_(Hands, Players, ID1).


% Main Loop
% ==================================================

%% +Agent.play() := true
% Starts the agent. A read-eval-print loop is presented to the user. The user
% uses the terminal to inform the agent of events in the game through English
% sentences. See `Agent.interpret()` for details on which sentences are
% understood by the agent.

Agent.play() := true :-
	catch((
		% The main interface is a simple read, eval, print loop.
		% Printing happens during the interpret call.
		(
			CMD = Agent.get_user_input(),
			NewState = Agent.interpret(CMD)
		;
			ansi_format([fg(red)], "That doesn't make sense to me.\n", []),
			NewState = Agent
		),
		!,

		% If our certainty is over 50%, we can make an accusation. Otherwise we
		% loop to keep listening for input. The only time the certainty is over
		% 50% is when there is exactly 1 solution (i.e. the certainty is 100%), but
		% this may change in the future.
		NewState.get_theory() = case(Certainty, [Suspect, Weapon, Room]),
		(Certainty > 0.5 ->
			write("I accuse "),
			print_card(Suspect),
			write(" with the "),
			print_card(Weapon),
			write(" in the "),
			print_card(Room),
			write("!\n")
		;
			NewState.play()
		)
	), user_quit, true).


%% +Agent.get_user_input() := -Statement
% Reads and parses input from the user.

Agent.get_user_input() := Statement :-
	write("> "),
	read_line_to_codes(user_input, Line),

	% Add the line to the readline history.
	% http://www.swi-prolog.org/pldoc/man?section=readline
	atom_codes(LineAtom, Line),
	rl_add_history(LineAtom),

	parse(Agent, Line, Statement).


% Interpreter
% ==================================================

%% +Agent.interpret(+Statement) := -NewState
% Given a Statement from the user, the agent records that information and takes
% appropriate action. Below is a list of statements understood by the Agent.
% Capital words are variables subject to interpretation by the parser, while
% lowercase words appear literally in the statement.
%
% "Player suggests Suspect in Room with Weapon.":
%     The agent commits to memory the current suggestion.
%
% "Player has Card.":
%     The agent takes note that the card is possesed by the given player.
%     The conjugated form "You have Card." should be used at the start of the
%     game to tell the agent which cards it posseses.
%
% "Player has a card.":
%     The agent take note that the player posseses at least one of the cards
%     from the current suggestion.
%
% "Player passes.":
%     The agent takes note that the player does not have any cards in the
%     current suggestion.
%
% "show a card.":
%     This statement alerts the agent that it is its turn to reveal a card. The
%     agent either responds with the card it wishes to reveal or responds that
%     it has no cards from the current suggestion.
%
% "show cases.":
%     This statement instructs the agent to reveal its current cases on the
%     game. The cases consist of possible solutions and the agent's certainty
%     about each one.
%
% "show status.":
%     This statement instructs the agent to reveal various information about the
%     current status of the game, including what the most recent suggestion was
%     and the contents of opponents hands.
%
% "move Distance from Location.":
%     This statement alerts the agent that it is its turn to move, and that it
%     may move a certain distance from the given location. If the agent can make
%     a suggestion after moving, it will do so.

Agent.interpret(Statement) := NewState :-
	agent(Agent),
	TmpState = Agent.interpret_(Statement),

	% If the current best theory has a certainty over 1%,
	% switch to full search mode.
	CurrentTheory = case(Certainty, _),
	CurrentTheory = TmpState.get_theory(),
	(Certainty > 0.01 ->
		NewState = TmpState.put(full_search, true)
	;
		NewState = TmpState
	).


% `Player suggests Suspect in Room with Weapon.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Player,
		suggests,
		suspect(_,SID),
		[
			in(room(_,RID)),
			with(weapon(_,WID))
		]
	),

	% Set the current suggestion.
	NewState = Agent.put(current_suggestion, suggestion(Player, SID, WID, RID)),
	!.


% `Player has Card.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Player,
		has,
		Card,
		[]
	),
	card(Card, ID),

	% Constrain the hand of player P to contain Card.
	element(_, Player.hand, ID),

	% We must clear the cache since we have new constraints.
	NewState = Agent.clear_cache().put(current_suggestion, _),
	!.


% `Player has a card.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Player,
		has,
		card,
		[]
	),

	% Constrain the hand of player P to contain at least one of the cards.
	Agent.current_suggestion = suggestion(_, SID, WID, RID),
	nonempty_intersection(Player.hand, [SID, WID, RID]),

	% We must clear the cache since we have new constraints.
	NewState = Agent.clear_cache().put(current_suggestion, _),
	!.


% `Player passes.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Player,
		passes,
		'',
		[]
	),
	Agent.current_suggestion = suggestion(_, SID, WID, RID),
	empty_intersection(Player.hand, [SID,WID,RID]),

	% We must clear the cache since we have new constraints.
	NewState = Agent.clear_cache(),
	!.


% `show a card.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Agent.self,
		shows,
		card,
		[]
	),

	% Reveal the card which has been already been revealed the most.
	nonvar(Agent.current_suggestion),
	Agent.current_suggestion = suggestion(_, S, W, R),
	label(Agent.self.hand),
	findall([Count,Card], (
		member(Card, [S,W,R]),
		nth1(Index, Agent.self.hand, Card),
		nth1(Index, Agent.shown_counts, Count)
	), Unsorted),
	sort(Unsorted, Backwards),
	reverse(Backwards, Sorted),
	Sorted = [[BestCount,_]|_],
	findall(Card, (
		member([BestCount,Card], Sorted)
	), Cards),
	random_select(Card, Cards, _),
	write("I reveal "),
	print_card(Card),
	write(".\n"),

	% Increment the count of times the card has been shown
	findall(Count, (
		nth1(Index, Agent.self.hand, X),
		(X = Card ->
			Count is BestCount + 1
		;
			nth1(Index, Agent.shown_counts, Count)
		)
	), NewCounts),
	NewState = Agent.put(shown_counts, NewCounts),
	!.

Agent.interpret_(Statement) := Agent :-
	Statement = statement(
		Agent.self,
		shows,
		card,
		[]
	),

	% SInce the previouse case didn't work, the agent hasn't anything to show.
	write("I have nothing to show.\n"),
	!.


% `show cases.`
% --------------------------------------------------
Agent.interpret_(Statement) := Agent :-
	Statement = statement(
		Agent.self,
		shows,
		cases,
		[]
	),

	Cases = Agent.findall_cases(),
	foreach(member(Case, Cases), (
		print_case(Case)
	)),
	!.


% `show status.`
% --------------------------------------------------
Agent.interpret_(Statement) := Agent :-
	Statement = statement(
		Agent.self,
		shows,
		status,
		[]
	),

	% Show the most recent suggestion if one exists.
	(nonvar(Agent.current_suggestion) ->
		Agent.current_suggestion = suggestion(P,S,W,R),
		(P = Agent.self ->
			write("I am suggesting ")
		;
			format("~w is suggesting ", [P.name])
		),
		print_card(S),
		write(" with "),
		print_card(W),
		write(" in "),
		print_card(R),
		write(".\n")
	; true),

	% If the entire contents of a player's hand is known, print it.
	forall(member(Player, Agent.players), (
		Hand = Player.hand,
		findnsols(2, Hand, label(Hand), Hands),
		length(Hands, L),
		(L = 1 ->
			label(Hand),
			(Player = Agent.self ->
				write("I have ")
			;
				format("~w has ", [Player.name])
			),
			print_hand(Hand),
			nl
		; true)
	)),
	!.


% `move Distance from Node.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Agent.self,
		moves,
		Distance,
		[
			from(Node)
		]
	),
	between(2, 12, Distance),
	node(Node),

	Cases = Agent.findall_cases(),
	findall(Destination, reachable(Destination, Node, Distance), Dests),

	(
		% The case where we can move to a room that could be a solution.
		member(case(BestCertainty,[_,_,RoomID]), Cases),
		member(room(_,RoomID), Dests)
	->
		write("I move to "),
		print_card(RoomID),
		findall(case(BestCertainty,[S,W,RoomID]), (
			member(case(BestCertainty,[S,W,RoomID]), Cases)
		), GoodCases),
		random_select(Case, GoodCases, _),
		Case = case(_,[SuspectID,WeaponID,RoomID]),
		write(".\nI suggest "),
		print_card(SuspectID),
		write(" with "),
		print_card(WeaponID),
		write(" in "),
		print_card(RoomID),
		write(".\n"),
		NewState = Agent.put(
			current_suggestion,
			suggestion(Agent.self, SuspectID, WeaponID, RoomID)
		),
		!
	;
		% The case where we have at least one room into which we can move.
		findall(room(Name,RoomID), member(room(Name,RoomID),Dests), RoomDests),
		length(RoomDests, NumRoomDests),
		NumRoomDests > 0
	->
		Rotation is Agent.order - 1,
		rotate(Rotation, Agent.players, PlayersByPenalty),
		findall([NegativePenalty,RoomID], (
			member(room(_,RoomID), RoomDests),
			nth0(PositivePenalty, PlayersByPenalty, Player),
			Hand = Player.hand,
			forall(labeling([ffc], Hand), (
				member(RoomID, Hand)
			)),
			NegativePenalty is 0 - PositivePenalty
		), RoomRanks),
		sort(RoomRanks, [[_Penalty,RoomID]|_]),
		write("I move to "),
		print_card(RoomID),
		Case = Agent.get_theory(),
		Case = case(_,[SuspectID,WeaponID,_]),
		write(".\nI suggest "),
		print_card(SuspectID),
		write(" with "),
		print_card(WeaponID),
		write(" in "),
		print_card(RoomID),
		write(".\n"),
		NewState = Agent.put(
			current_suggestion,
			suggestion(Agent.self, SuspectID, WeaponID, RoomID)
		),
		!
	;
		% The case where we must move into the open area.
		Agent.get_theory() = case(_,[_,_,TargetID]),
		TargetRoom = room(_,TargetID),
		call(TargetRoom), % Fill in the var in the compound term.
		path(Node, TargetRoom, Path),
		findall([InverseDistance,CommonNode], (
			member(CommonNode, Path),
			reachable(CommonNode, Node, Distance),
			distance_estimate(Node, CommonNode, DistEstimate),
			InverseDistance is 0 - DistEstimate
		), CommonNodes),
		sort(CommonNodes, [[_,point(X,Y)]|_]),
		format("I move to (~w,~w).\n", [X,Y]),
		NewState = Agent,
		!
	).


% `refer to Player as Alias.`
% --------------------------------------------------
Agent.interpret_(Statement) := NewState :-
	Statement = statement(
		Agent.self,
		refers,
		'',
		[
			as(Alias),
			to(Player)
		]
	),

	NewPlayer = Player.put(name, Alias),
	Player = Agent.select_player(_,OtherPlayers),
	NewState = Agent.put(players, [NewPlayer|OtherPlayers]),
	!.




% `quit.`
% --------------------------------------------------
Agent.interpret_(Statement) := Agent :-
	Statement = statement(
		Agent.self,
		quits,
		'',
		[]
	),
	!,
	throw(user_quit).


% `debug.`
% --------------------------------------------------
Agent.interpret_(Statement) := Agent :-
	Statement = statement(
		Agent.self,
		debugs,
		'',
		[]
	),

	write(Agent.current_suggestion),
	nl,
	!,
	trace.


% Helper Methods
% ==================================================

%% +Agent.findall_cases() := -Cases
% Gets the list of all possible solutions sorted by certainty. Solutions are
% represented by compound terms of the form
%     `case(Certainty, [Suspect,Weapon,Room])`
% The solutions are cached, and the cache must be cleared when new information
% is given to the user. See `Agent.clear_cache()`.

Agent.findall_cases() := Agent.cache :-
	agent(Agent),
	nonvar(Agent.cache),
	!.

Agent.findall_cases() := Agent.cache :-
	agent(Agent),
	var(Agent.cache),

	% Find all combinations
	findall([S,W,R], (
		Agent.case = [S,W,R],
		( Agent.full_search ->
			labeling([ffc], Agent.cards)
		;
			label(Agent.case)
		)
	), Cases),

	% Calculate the certainty for each case.
	% Store the results in `Agent.cache`.
	length(Cases, NumCases),
	findall(S, member([S,_,_], Cases), Suspects),
	findall(W, member([_,W,_], Cases), Weapons),
	findall(R, member([_,_,R], Cases), Rooms),
	counts(Suspects, SuspectCounts),
	counts(Weapons, WeaponCounts),
	counts(Rooms, RoomCounts),
	findall(case(Certainty, [S,W,R]), (
		member([S,SuspectCount], SuspectCounts),
		member([W,WeaponCount], WeaponCounts),
		member([R,RoomCount], RoomCounts),
		member([S,W,R], Cases),
		Certainty is (SuspectCount/NumCases)
		           * (WeaponCount/NumCases)
		           * (RoomCount/NumCases)
	), CacheUnsorted),
	sort(CacheUnsorted, CacheBackwards),
	reverse(CacheBackwards, Agent.cache).


%% +Agent.select_player(?NameOrID, ?Rest) := ?Player
% True when Player is a member of Agent.players with the given NameOrID and Rest
% is the list of other players.

Agent.select_player(Ref, Rest) := P :-
	agent(Agent),
	player(P),
	(P.id = Ref ; P.name = Ref),
	select(P, Agent.players, Rest).


%% +Agent.get_theory() := -Case
% Returns the current solution with the highest certainty.

Agent.get_theory() := Case :-
	agent(Agent),
	[Case|_] = Agent.findall_cases().


%% +Agent.get_certainty(+Suspect, +Weapon, +Room) := -Certainty
% Get the certainty of a solution. This fully populates the solution cache.

Agent.get_certainty(S, W, R) := Certainty :-
	agent(Agent),
	Cases = Agent.findall_cases(),
	member(case(Certainty, [S, W, R]), Cases).


%% +Agent.clear_cache() := -NewState
% Clears the solution cache.

Agent.clear_cache() := Agent.put(cache, _) :- agent(Agent).


%% print_case(+Case)
% Prints a solution of the form `case(Certainty, [Suspect,Weapon,Room])`.

print_case(case(Certainty, [SID,WID,RID])) :-
	CertaintyPercent is Certainty * 100,
	suspect(S, SID),
	weapon(W, WID),
	room(R, RID),
	write("I am "),
	ansi_format([fg(cyan)], "~3f%", [CertaintyPercent]),
	write(" certain it was "),
	print_card(S),
	write(" with "),
	print_card(W),
	write(" in "),
	print_card(R),
	write(".\n").


%% print_hand(+Hand)
% Prints a list of cards.

print_hand([X]) :-
	write("and "),
	print_card(X),
	!.
print_hand([H|T]) :-
	print_card(H),
	write(", "),
	print_hand(T).
