%Tori Windrich - taw170030 - Assignment 3 Task 2

%Deal the top card from the passed in Deck
deal_card([Card|Deck],Card,Deck).

%get a shuffled deck
get_deck(ShuffledDeck) :-
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
	[king,heart],[king,diamond],[king,club],[king,spade]],ShuffledDeck). %Get a random permutation of all 52 cards, store the shuffled cards in Deck


% Print out the number of each hand rank win for player 1, as well as
% the probability based on the number of wins player had for the rank
print_P1_results([SF,Th,S,F,P,H],[SFW,TW,SW,FW,PW,HW]) :-
	write("STATS FOR PLAYER 1:"), nl,
	write("Straight flushes win Freq: "), print(SFW),
	(SF \= 0, SFP is SFW/SF*100; SF = 0, SFP is 0),
	write("\t\tWin Probability: "), print(SFP), write("%"), nl,
	write("Three of a kind win Freq: "), print(TW),
	(Th \= 0, ThP is TW/Th*100; Th = 0, ThP is 0),
	write("\t\tWin Probability: "), print(ThP), write("%"), nl,
	write("Straight win Freq: "), print(SW),
	(S \= 0, SP is SW/S*100; S = 0, SP is 0),
	write("\t\tWin Probability: "), print(SP), write("%"),  nl,
	write("Flush win Freq: "), print(FW),
	(F \= 0, FP is FW/F*100; F = 0, FP is 0),
	write("\t\tWin Probability: "), print(FP), write("%"),  nl,
	write("Pair win Freq: "), print(PW),
	(P \= 0, PP is PW/P*100; P = 0, PP is 0),
	write("\t\tWin Probability: "), print(PP), write("%"),  nl,
	write("High Card win Freq: "), print(HW),
	(H \= 0, HP is HW/H*100; H = 0, HP is 0),
	write("\t\tWin Probability: "), print(HP), write("%"),  nl.

% prints count of each rank gotten by player 2 for each rank gotten by
% player 1
print_P2_based_on_P1([P1,SF,T,S,F,P,H]) :-
	write("When Player 1 Got a "), write(P1), write(", Player two got:"), nl,
	write(SF), write(" straight flushes."), nl,
	write(T), write(" three of a kinds."), nl,
	write(S), write(" straights."), nl,
	write(F), write(" flushes."), nl,
	write(P), write(" pairs."), nl,
	write(H), write(" high cards."), nl.

print_first_results([SF,T,S,F,P,H]) :-
	print_P2_based_on_P1(SF),
	print_P2_based_on_P1(T),
	print_P2_based_on_P1(S),
	print_P2_based_on_P1(F),
	print_P2_based_on_P1(P),
	print_P2_based_on_P1(H).


% For the passed in integer N, run N games, and then print out the
% resulting hand ranks.
poker(N) :-
	write("BEGINNING "), print(N), write(" GAMES."), nl,
	poker(N,[[straight_flush,0,0,0,0,0,0],[three_of_a_kind,0,0,0,0,0,0],[straight,0,0,0,0,0,0],[flush,0,0,0,0,0,0],[pair,0,0,0,0,0,0],[high_cards,0,0,0,0,0,0]],[0,0,0,0,0,0],[0,0,0,0,0,0],[SF,Th,S,F,P,H],P1Results,P1Wins), %hand ranks start as all zeros, but Newresults will hold the resulting number of ranks
	write("GAMES COMPLETE. RESULTS:"), nl,
	print_first_results([SF,Th,S,F,P,H]),
	print_P1_results(P1Results,P1Wins).

poker(0,Results,P1Results,PWins,Results,P1Results,PWins). %If N is zero, return current Results as third parameter
poker(N,Results,P1Results,P1Wins,X,Y,Z) :- %if N is not zero, we take in the current Results, and we don't care what X is
    N > 0,
    get_deck(Deck),
    game(Deck,Results,P1Results,P1Wins,Newresults,NewP1Results,NewP1Wins),
    N2 is N - 1, %decrease N by one, store in N2
    poker(N2,Newresults,NewP1Results,NewP1Wins,X,Y,Z). %call Poker again with N-1

game(Deck, Results,P1Results,P1Wins, Newresults, NewP1Results,NewP1Wins) :-
    deal_card(Deck,Card1,NewDeck), %deal the top card of the deck, return the rest in NewDeck
    deal_card(NewDeck,Card2,NewDeck2), %deal the top card of the NewDeck, return threst in NewDeck2
    deal_card(NewDeck2,Card3,NewDeck3), %deal the top card of the NewDeck2, throw out the rest since we don't need it for anything in this version
    deal_card(NewDeck3,Card4,NewDeck4),
    deal_card(NewDeck4,Card5,NewDeck5),
    deal_card(NewDeck5,Card6,_),
    Player1 = [Card1,Card3,Card5], %put the cards into Hand
    Player2 = [Card2,Card4,Card6],
    sort_hand(Player1,SortedPlayer1), %sort the Hand to make it easier to determine hand ranks
    sort_hand(Player2,SortedPlayer2),
    write("Player 1: "), print(SortedPlayer1), nl,
    write("Player 2: "), print(SortedPlayer2), nl,
    determine_hand(SortedPlayer1, P1Result), %determine the result of the SortedHand, store in Result
    write("Player 1 Hand Rank: "), print(P1Result), nl,
    determine_hand(SortedPlayer2, P2Result),
    write("Player 2 Hand Rank: "), print(P2Result), nl,
    get_winner(SortedPlayer1,SortedPlayer2,P1Result,P2Result,WinnerHand),
    (WinnerHand = SortedPlayer1, Winner = "Player One", Win = yes;
    WinnerHand = SortedPlayer2, Winner = "Player Two", Win = no),
    get_P1(P1Result,Win,P1Results,P1Wins,NewP1Results,NewP1Wins),
    write("Winner: "), write(Winner), write(" with hand: "),
    print(WinnerHand), nl,
    update_rank_results(P1Result,P2Result,Results,Newresults).

%used to get the number of each rank for player win and increment wins
get_P1(straight_flush, Win, [SF,Th,S,F,P,H], [SFW,ThW,SW,FW,PW,HW], [SF2,Th,S,F,P,H],[SFW2,ThW,SW,FW,PW,HW]) :- SF2 is SF+1, (Win = yes, SFW2 is SFW+1; Win = no, SFW2 is SFW).
get_P1(three_of_a_kind, Win, [SF,Th,S,F,P,H],[SFW,ThW,SW,FW,PW,HW], [SF,Th2,S,F,P,H],[SFW,ThW2,SW,FW,PW,HW]) :- Th2 is Th+1, (Win = yes, ThW2 is ThW+1; Win = no, ThW2 is ThW).
get_P1(straight, Win, [SF,Th,S,F,P,H],[SFW,ThW,SW,FW,PW,HW], [SF,Th,S2,F,P,H],[SFW,ThW,SW2,FW,PW,HW]) :- S2 is S+1, (Win = yes, SW2 is SW+1; Win = no, SW2 is SW).
get_P1(flush, Win, [SF,Th,S,F,P,H],[SFW,ThW,SW,FW,PW,HW], [SF,Th,S,F2,P,H], [SFW,ThW,SW,FW2,PW,HW]) :- F2 is F+1, (Win = yes, FW2 is FW+1; Win = no, FW2 is FW).
get_P1(pair, Win, [SF,Th,S,F,P,H],[SFW,ThW,SW,FW,PW,HW], [SF,Th,S,F,P2,H], [SFW,ThW,SW,FW,PW2,HW]) :- P2 is P+1, (Win = yes, PW2 is PW+1; Win = no, PW2 is PW).
get_P1(high_card, Win, [SF,Th,S,F,P,H],[SFW,ThW,SW,FW,PW,HW], [SF,Th,S,F,P,H2], [SFW,ThW,SW,FW,PW,HW2]) :- H2 is H+1, (Win = yes, HW2 is HW+1; Win = no, HW2 is HW).

%used to update player two's rank based on player one's rank
update_rank_results(straight_flush,P2Rank,[SF,T,S,F,P,H],[SF2,T,S,F,P,H]) :-
	get_hand_rank(P2Rank, SF, SF2).
update_rank_results(three_of_a_kind,P2Rank,[SF,T,S,F,P,H],[SF,T2,S,F,P,H]) :-
	get_hand_rank(P2Rank, T, T2).
update_rank_results(straight,P2Rank,[SF,T,S,F,P,H],[SF,T,S2,F,P,H]) :-
	get_hand_rank(P2Rank, S, S2).
update_rank_results(flush,P2Rank,[SF,T,S,F,P,H],[SF,T,S,F2,P,H]) :-
	get_hand_rank(P2Rank, F, F2).
update_rank_results(pair,P2Rank,[SF,T,S,F,P,H],[SF,T,S,F,P2,H]) :-
	get_hand_rank(P2Rank, P, P2).
update_rank_results(high_card,P2Rank,[SF,T,S,F,P,H],[SF,T,S,F,P,H2]) :-
	get_hand_rank(P2Rank, H, H2).


% depending on what the result (first parameter), take the results
% (second parameter), and add one of the appropriate value and return in
% 3rd parameter
get_hand_rank(straight_flush, [R,SF,Th,S,F,P,H], [R,SF2,Th,S,F,P,H]) :- SF2 is SF+1.
get_hand_rank(three_of_a_kind, [R,SF,Th,S,F,P,H], [R,SF,Th2,S,F,P,H]) :- Th2 is Th+1.
get_hand_rank(straight, [R,SF,Th,S,F,P,H], [R,SF,Th,S2,F,P,H]) :- S2 is S+1.
get_hand_rank(flush, [R,SF,Th,S,F,P,H], [R,SF,Th,S,F2,P,H]) :- F2 is F+1.
get_hand_rank(pair, [R,SF,Th,S,F,P,H], [R,SF,Th,S,F,P2,H]) :- P2 is P+1.
get_hand_rank(high_card, [R,SF,Th,S,F,P,H], [R,SF,Th,S,F,P,H2]) :- H2 is H+1.

%determines the winner (makes use of modified code from sample code)
get_winner(Player1,Player2,PR1,PR2,Winner) :-
	 beats(PR1, PR2, Verdict),
	 (Verdict = PR1, Winner = Player1;
	 Verdict = PR2, Winner = Player2;
	 Verdict = tie, tiebreak(PR1, Player1, Player2, SortedWinner),
	 (SortedWinner = left, Winner = Player1 ;
	 SortedWinner = right, Winner = Player2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Tiebreaks - from sample code with some modifications
tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
tiebreak(flush, H1, H2, Winner)           :- tiebreak(high_card, H1, H2, Winner).
tiebreak(straight, H1, H2, Winner)        :- higher_last_card(H1, H2, Winner).
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).

tiebreak(pair, H1, H2, Winner) :-
  isolate_pair(H1, [PairCard1,_], Rst1),
  isolate_pair(H2, [PairCard2,_], Rst2),
  (beats_with_hand(H1, PairCard1, H2, PairCard2, Winner), Winner \= tie ;
   tiebreak(high_card, Rst1, Rst2, Winner)).

tiebreak(high_card, H1, H2, X) :-
  reverse(H1, RevH1),
  reverse(H2, RevH2),
  highest_card_chain(RevH1, RevH2, X).

beats_with_hand(_, C1, _, C2, X) :-
  beats(C1, C2, C1), X = left ;
  beats(C1, C2, C2), X = right ;
  X = tie.

isolate_pair(Hand, Pair, Rst) :-
  [[V1,S1],[V2,S2],[V3,S3]] = Hand,
  (V1 = V2, Pair = [[V1,S1],[V2,S2]], Rst = [[V3,S3]] ;
   V2 = V3, Pair = [[V3,S3],[V2,S2]], Rst = [[V1,S1]]).


highest_card_chain([H1|T1], [H2|T2], X) :-
  beats(H1,H2,Verdict),
  (Verdict = H1, X = left ;
   Verdict = H2, X = right ;
   Verdict = tie, highest_card_chain(T1,T2,X)).

higher_last_card(H1,H2,Winner) :-
  H1 = [_,_,[V1,_]],
  H2 = [_,_,[V2,_]],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left ;
   Higher = V2, Winner = right).

higher_middle_card(H1, H2, Winner) :-
  H1 = [_,[V1,_],_],
  H2 = [_,[V2,_],_],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left;
   Higher = V2, Winner = right).

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
?- poker(10000).

