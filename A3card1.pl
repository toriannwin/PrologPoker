%Tori Windrich - taw170030 - Assignment 3 Task 1

%Deal the top card from the passed in Deck
deal_card([Card|Deck],Card,Deck).

% Print out the number of each hand rank, as well as the probability
% based on the N number of runs
printResults(N,[SF,Th,S,F,P,H]) :-
	write("Straight flushes Freq: "), print(SF),
	SFP is SF/N*100,
	write("\t\tProbability: "), print(SFP), write("%"), nl,
	write("Three of a kind Freq: "), print(Th),
	ThP is Th/N*100,
	write("\t\tProbability: "), print(ThP), write("%"), nl,
	write("Straight Freq: "), print(S),
	SP is S/N*100,
	write("\t\tProbability: "), print(SP), write("%"),  nl,
	write("Flush Freq: "), print(F),
	FP is F/N*100,
	write("\t\tProbability: "), print(FP), write("%"),  nl,
	write("Pair Freq: "), print(P),
	PP is P/N*100,
	write("\t\tProbability: "), print(PP), write("%"),  nl,
	write("High Card Freq: "), print(H),
	HP is H/N*100,
	write("\t\tProbability: "), print(HP), write("%"),  nl.

% For the passed in integer N, run N games, and then print out the
% resulting hand ranks.
poker(N) :-
	write("BEGINNING "), print(N), write(" GAMES."), nl,
	poker(N,[0,0,0,0,0,0],Newresults), %hand ranks start as all zeros, but Newresults will hold the resulting number of ranks
	write("GAMES COMPLETE. RESULTS:"), nl,
	printResults(N,Newresults). %SF, Th, S, F, P, H - the order of the ranks in the list

poker(0,Results,Results). %If N is zero, return current Results as third parameter
poker(N,Results,X) :- %if N is not zero, we take in the current Results, and we don't care what X is
    random_permutation([
	[ace,heart],[ace,diamond],[ace,club],[ace,spade],
	[2,heart],[2,diamond],[2,club],[2,spade],
	[3,heart],[3,diamond],[3,club],[3,spade],
	[4,heart],[4,diamond],[4,club],[4,spade],
	[5,heart],[5,diamond],[5,club],[5,spade],
	[6,heart],[6,diamond],[6,club],[6,spade],
	[7,heart],[7,diamond],[7,club],[7,spade],
	[8,heart],[8,diamond],[8,club],[8,spade],
	[9,heart],[9,diamond],[9,club],[9,spade],
	[10,heart],[10,diamond],[10,club],[10,spade],
	[jack,heart],[jack,diamond],[jack,club],[jack,spade],
	[queen,heart],[queen,diamond],[queen,club],[queen,spade],
	[king,heart],[king,diamond],[king,club],[king,spade]],Deck), %Get a random permutation of all 52 cards, store the shuffled cards in Deck
    deal_card(Deck,Card1,NewDeck), %deal the top card of the deck, return the rest in NewDeck
    deal_card(NewDeck,Card2,NewDeck2), %deal the top card of the NewDeck, return the rest in NewDeck2
    deal_card(NewDeck2,Card3,_), %deal the top card of the NewDeck2, throw out the rest since we don't need it for anything in this version
    Hand = [Card1,Card2,Card3], %put the cards into Hand
    sort_hand(Hand,SortedHand), %sort the Hand to make it easier to determine hand ranks
    write("Hand: "), print(SortedHand), nl,
    determine_hand(SortedHand, Result), %determine the result of the SortedHand, store in Result
    write("\tHand Rank: "), print(Result), nl,
    get_hand_rank(Result,Results,Newresults), %get the new results
    N2 is N-1, %decrease N by one, store in N2
    poker(N2,Newresults,X). %call Poker again with N-1

% depending on what the result (first parameter), take the results
% (second parameter), and add one of the appropriate value and return in
% 3rd parameter
get_hand_rank(straight_flush, [SF,Th,S,F,P,H], [SF2,Th,S,F,P,H]) :- SF2 is SF+1.
get_hand_rank(three_of_a_kind, [SF,Th,S,F,P,H], [SF,Th2,S,F,P,H]) :- Th2 is Th+1.
get_hand_rank(straight, [SF,Th,S,F,P,H], [SF,Th,S2,F,P,H]) :- S2 is S+1.
get_hand_rank(flush, [SF,Th,S,F,P,H], [SF,Th,S,F2,P,H]) :- F2 is F+1.
get_hand_rank(pair, [SF,Th,S,F,P,H], [SF,Th,S,F,P2,H]) :- P2 is P+1.
get_hand_rank(high_card, [SF,Th,S,F,P,H], [SF,Th,S,F,P,H2]) :- H2 is H+1.

%If the suit is the same, and the ranks are in order, straight flush
determine_hand([[A,X],[B,X],[C,X]], straight_flush) :-
   successor(C,B), successor(B,A).

%if the three ranks are the same, three of a kind
determine_hand([[A,_],[B,_],[C,_]], three_of_a_kind) :- A = B, A = C.

%if the suits are all the same, flush
determine_hand([[_,X],[_,X],[_,X]], flush).

%if the ranks are in order, straight
determine_hand([[A,_],[B,_],[C,_]], straight) :-
  successor(C,B), successor(B,A).

%if two of the ranks match, pair
determine_hand([[A,_],[B,_],[C,_]], pair) :-
  A = B; B = C.

%otherwise, high card
determine_hand(_,high_card).

%sort the hand
sort_hand([], []).
sort_hand([H|T], Sorted) :-
  filter_by_high_card(H,T,Lower,Higher),
  sort_hand(Lower,SortedLower),
  sort_hand(Higher,SortedHigher),
  append(SortedLower, [H|SortedHigher], Sorted).


filter_by_high_card(_, [], [], []).
filter_by_high_card(Pivot, [H|T], [H|Lower], Higher) :-
  beats(Pivot,H,Z),
  (Z = Pivot ; Z = tie),
  filter_by_high_card(Pivot, T, Lower, Higher).
filter_by_high_card(Pivot, [H|T], Lower, [H|Higher]) :-
  beats(Pivot,H,H),
  filter_by_high_card(Pivot, T, Lower, Higher).

%determines what value beats what (based on high card)
beats([V,_],[V,_],tie).
beats([V1,S],[V2,_],[V1,S]) :- value_greater_than(V1,V2).
beats([V1,_],[V2,S],[V2,S]) :- value_greater_than(V2,V1).

beats(X,X,tie).
beats(X,Y,X) :- value_greater_than(X,Y).
beats(X,Y,Y) :- value_greater_than(Y,X).

%determine what values are greater than what
value_greater_than(X,Y) :-
  successor(X,P),
  (Y = P;
  value_greater_than(P,Y)).

%determines what is a successor of what
successor(straight_flush, three_of_a_kind).
successor(three_of_a_kind, straight).
successor(straight, flush).
successor(flush, pair).
successor(pair, high_card).

successor(ace,king).     successor(king,queen).   successor(queen,jack).
successor(jack,10).      successor(10,9).         successor(9,8).
successor(8,7).          successor(7,6).          successor(6,5).
successor(5,4).          successor(4,3).          successor(3,2).


?- poker(10).
?- poker(100).
?- poker(1000).
?- poker(2000).
?- poker(5000).
?- poker(10000).
