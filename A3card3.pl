%Tori Windrich - taw170030 - Assignment 3 Task 3

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

print_results([P1,TP,FP,TN,FN]) :-
    Total is TP + FP + TN + FN,
    (Total \= 0, TPP is(TP+TN)/Total*100; Total = 0, TPP is 0),
    write("When Player 1 Got a "), write(P1), nl,
    write("\tThere were "), print(TP), write(" true positives."), nl,
    write("\tThere were "), print(FP), write(" false positives."), nl,
    write("\tThere were "), print(TN), write(" true negatives."), nl,
    write("\tThere were "), print(FN), write(" false negatives."), nl,
    write("\tThe precision rate is "), print(TPP), write("%"), nl.
print_all_results([SF,Th,S,F,P,H]) :-
    print_results(SF),
    print_results(Th),
    print_results(S),
    print_results(F),
    print_results(P),
    print_results(H).

% For the passed in integer N, run N games, and then print out the
% resulting hand ranks.
poker(N) :-
	write("BEGINNING "), print(N), write(" GAMES."), nl,
	poker(N,[[straight_flush,0,0,0,0],[three_of_a_kind,0,0,0,0],[straight,0,0,0,0],[flush,0,0,0,0],[pair,0,0,0,0],[high_cards,0,0,0,0]],Results), %hand ranks start as all zeros, but Newresults will hold the resulting number of ranks
	write("GAMES COMPLETE. RESULTS:"), nl,
	print_all_results(Results).

poker(0,Results,Results). %If N is zero, return current Results as third parameter
poker(N,Results,X) :- %if N is not zero, we take in the current Results, and we don't care what X is
    N > 0,
    get_deck(Deck),
    game(Deck,Results,Newresults),
    N2 is N - 1, %decrease N by one, store in N2
    poker(N2,Newresults,X). %call Poker again with N-1

game(Deck, Results,Newresults) :-
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
    get_prediction(P1Result,Prediction),
    write("Prediction: "), write(Prediction), nl,
    get_winner(SortedPlayer1,SortedPlayer2,P1Result,P2Result,WinnerHand),
    (WinnerHand = SortedPlayer1, Winner = "Player One", Win = win;
    WinnerHand = SortedPlayer2, Winner = "Player Two", Win = lose),
    write("Winner: "), write(Winner), write(" with hand: "),
    print(WinnerHand), nl,
    get_P1(P1Result,Prediction,Win,Results,Newresults).

%These guesses are based on the win frequency determined from Task #2
get_prediction(Rank, Prediction) :-
    (Rank = straight_flush, Prediction = win;
    Rank = three_of_a_kind, Prediction = win;
    Rank = straight, straight_chance(Prediction);
    Rank = flush, flush_chance(Prediction);
    Rank = pair, pair_chance(Prediction);
    Rank = high_card, high_card_chance(Prediction)).

% Determines which sublist needs to be modified (depends on the rank
% passed in)
get_P1(straight_flush, Prediction, Actual, [SF,Th,S,F,P,H], [SF2,Th,S,F,P,H]) :-
    incr_P1(Prediction,Actual,SF,SF2).
get_P1(three_of_a_kind, Prediction, Actual, [SF,Th,S,F,P,H], [SF,Th2,S,F,P,H]) :-
    incr_P1(Prediction,Actual,Th,Th2).
get_P1(straight, Prediction, Actual, [SF,Th,S,F,P,H], [SF,Th,S2,F,P,H]) :-
    incr_P1(Prediction,Actual,S,S2).
get_P1(flush, Prediction, Actual, [SF,Th,S,F,P,H], [SF,Th,S,F2,P,H]) :-
    incr_P1(Prediction,Actual,F,F2).
get_P1(pair, Prediction, Actual, [SF,Th,S,F,P,H], [SF,Th,S,F,P2,H]) :-
    incr_P1(Prediction,Actual,P,P2).
get_P1(high_card, Prediction, Actual, [SF,Th,S,F,P,H], [SF,Th,S,F,P,H2]) :-
    incr_P1(Prediction,Actual,H,H2).


%increase the correct value TP,TN,FP,FN
incr_P1(win, win, [R,TP,TN,FP,FN],[R,TP2,TN,FP,FN]) :- TP2 is TP+1.
incr_P1(lose,lose, [R,TP,TN,FP,FN],[R,TP,TN2,FP,FN]) :- TN2 is TN+1.
incr_P1(win,lose, [R,TP,TN,FP,FN],[R,TP,TN,FP2,FN]) :- FP2 is FP+1.
incr_P1(lose,win, [R,TP,TN,FP,FN],[R,TP,TN,FP,FN2]) :- FN2 is FN+1.


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


% Predicts win or lose based on the win probability calculated in
% A3card2
straight_chance(Prediction) :-
    random_member(Prediction,[win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,lose,lose,lose]).
flush_chance(Prediction) :-
    random_member(Prediction,[win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,lose,lose,lose,lose,lose,lose]).

pair_chance(Prediction) :-
    random_member(Prediction,[win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose]).

high_card_chance(Prediction) :-
    random_member(Prediction,[win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,win,win,win,
                              win,win,win,win,win,win,win,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose,
                              lose,lose,lose,lose,lose,lose,lose,lose,lose,lose]).

?- poker(10).
?- poker(100).
?- poker(1000).
?- poker(10000).

