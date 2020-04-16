%Tori Windrich - taw170030 - Assignment 3 Task 4

print_results(P1Money,P2Money,[PlayerOne, PlayerTwo,Ties]) :-
    write("Player One won "), print(PlayerOne), write(" times."), nl,
    write("Player Two won "), print(PlayerTwo), write(" times."), nl,
    write("The Players Folded/Tied "), print(Ties), write(" times."), nl,
    write("Player One ended with $"), print(P1Money), write("."), nl,
    write("Player Two ended with $"), print(P2Money), write("."), nl,
    (P1Money > P2Money, write("Player One Wins!!"), nl;
     P1Money < P2Money, write("Player Two Wins!!"), nl;
     P1Money = P2Money, write("The players have tied.")), nl.

% For the passed in integer N, run N games, and then print out the
% resulting hand ranks.
poker(M) :-
	atom_concat(a3card4out,M,Filename),
	string_concat(Filename,'.txt',File),
	open(File,write,Out),
	write("BEGINNING GAMES, BOTH PLAYERS STARTING WITH $"),
        print(M), write("."), nl,
	poker(Out,M,M,0,[0,0,0],P1Money,P2Money,WinResults,yes),
        write("GAMES COMPLETE. RESULTS:"), nl,
	print_results(P1Money,P2Money,WinResults),
	close(Out).

%handles the looping of the games
%Pass indicates if the players have enough money to move on to another game
%Wins keeps track of the number of wins and ties of each player.
poker(_,P1Money,P2Money,100,Wins,P1Money,P2Money,Wins,_). %If N is greater than 99, return all current results
poker(_,P1Money,P2Money,_,Wins,P1Money,P2Money,Wins,no).
poker(Out,P1Money,P2Money,N,Wins,X,Y,Z,Pass) :- %if N is not zero, we take in the current Results, and we don't care what X is
    P1Money > 10, P2Money > 10,
    get_deck(Deck),
    initial_bets(P1Money,P2Money,B1P1Money,B1P2Money,Pass),
    get_print(B1P1Money,B1P2Money,N,Print),
    (Pass = yes, game(Out,Print,Deck,B1P1Money,B1P2Money,Wins,NewP1Money,NewP2Money,NewWins,NewPass);
     Pass = no, NewP1Money is P1Money, NewP2Money is P2Money, NewWins = Wins),
    N2 is N+1, %increase N by one, store in N2
    poker(Out,NewP1Money,NewP2Money,N2,NewWins,X,Y,Z,NewPass). %call Poker again with N+1

%call the essential game predicates
game(Out,Print,Deck,P1Money,P2Money,Wins,P1NewMoney,P2NewMoney,NewWins,Pass) :-
    deal_hands(Out,Print,Deck,Player1,Player2),
    determine_all_hands(Out,Print,Player1,Player2,P1Result,P2Result),
    do_bets(Out,Print,P1Money,P1Result,P2Money,P2Result,P1NM,P2NM,FirstPass,Loser,Pot),
    get_winner(Out,Print,FirstPass,Loser,Player1,Player2,P1Result,P2Result,P1NM,P2NM,Pot,Wins,P1NewMoney,P2NewMoney,NewWins,Pass).

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

%deduct the initial $10 from each player and determine if the players have enough money
initial_bets(P1Money,P2Money,NewP1Money,NewP2Money,Pass) :-
    (P1Money > 10, NewP1Money is P1Money - 10, Pass = yes;
     P1Money < 10, NewP1Money is P1Money, Pass = no),
    (P2Money > 10, NewP2Money is P2Money - 10, Pass = yes;
     P2Money < 10, NewP2Money is P2Money, Pass = no).

%determine if it's time to print out the output
get_print(_,_,0,yes).
get_print(_,_,99,yes).
get_print(P1Money,P2Money,_,Print) :- (P1Money < 11, P2Money > 11, Print = yes;
				       P1Money > 11, P2Money < 11, Print = yes;
				       P1Money < 11, P2Money < 11, Print = yes;
				       P1Money > 11, P2Money > 11, Print = no;
				       P1Money = 11, P2Money = 11, Print = no).

%Deal hands for both players
deal_hands(Out,Print,Deck,SP1,SP2) :-
    deal_card(Deck,Card1,NewDeck), %deal the top card of the deck, return the rest in NewDeck
    deal_card(NewDeck,Card2,NewDeck2), %deal the top card of the NewDeck, return threst in NewDeck2
    deal_card(NewDeck2,Card3,NewDeck3), %deal the top card of the NewDeck2, throw out the rest since we don't need it for anything in this version
    deal_card(NewDeck3,Card4,NewDeck4),
    deal_card(NewDeck4,Card5,NewDeck5),
    deal_card(NewDeck5,Card6,_),
    PlayerOne = [Card1,Card3,Card5], %put the cards into Hand
    PlayerTwo = [Card2,Card4,Card6],
    sort_hand(PlayerOne,SortedPlayer1), %sort the Hand to make it easier to determine hand ranks
    sort_hand(PlayerTwo,SortedPlayer2),
    print_hands(Out,Print,SortedPlayer1,SortedPlayer2,SP1,SP2).

%determine the ranks of the hands of both players
determine_all_hands(Out,Print,Player1,Player2,Player1Rank,Player2Rank) :-
    determine_hand(Player1,P1Rank),
    determine_hand(Player2,P2Rank),
   % write("\tDetermined Hands"), write(Player1Rank), write(Player2Rank), nl,
    print_ranks(Out,Print,P1Rank,P2Rank,Player1Rank,Player2Rank).

%get the betting amount for this game
do_bets(Out,Print,P1Money,P1Rank,P2Money,P2Rank,P1NewMoney,P2NewMoney,Pass,ReturnLoser,Pot) :-
    bet(P1Money,P1Rank,P1WantedBet),
    bet(P2Money,P2Rank,P2WantedBet),
    Options = [P1WantedBet,P2WantedBet],
    max_member(FinalBet, Options),
    P1AfterBetMoney is P1Money - FinalBet,
    P2AfterBetMoney is P2Money - FinalBet,
   (P1AfterBetMoney < 0, FinalBet \= 0, Pass = no, Loser = p1, P1NewMoney is P1Money, P2NewMoney is P2Money, Pot is 0;
     P2AfterBetMoney < 0, FinalBet \= 0, Pass = no, Loser = p2, P1NewMoney is P1Money, P2NewMoney is P2Money, Pot is 0;
     P1AfterBetMoney >= 0, P2AfterBetMoney >= 0, FinalBet \= 0, Pot is FinalBet+FinalBet+20, Pass = yes, Loser = none, P1NewMoney is P1AfterBetMoney, P2NewMoney is P2AfterBetMoney;
     FinalBet = 0, Loser = both, P1NewMoney is P1Money, P2NewMoney is P2Money, Pot is 0),
     print_bets(Out,Print,Loser,FinalBet,ReturnLoser,_).

%uses prediction to then determine how much should be bet based on the hand
bet(Money,Rank,WantedBet2) :-
   (Rank = straight_flush, Prediction = win;
    Rank = three_of_a_kind, Prediction = win;
    Rank = straight, straight_chance(Prediction);
    Rank = flush, flush_chance(Prediction);
    Rank = pair, pair_chance(Prediction);
    Rank = high_card, high_card_chance(Prediction)),
   (Rank = straight_flush, Prediction = win, WantedBet is 10;
    Rank = three_of_a_kind, Prediction = win, WantedBet is 9;
    Rank = straight, Prediction = win, WantedBet is 8;
    Rank = straight, Prediction = lose, WantedBet is 6;
    Rank = flush, Prediction = win, WantedBet is 7;
    Rank = flush, Prediction = lose, WantedBet is 5;
    Rank = pair, Prediction = win, WantedBet is 4;
    Rank = pair, Prediction = lose, WantedBet is 2;
    Rank = high_card, Prediction = win, WantedBet is 1;
    Rank = high_card, Prediction = lose, WantedBet is 0),
   (Money < WantedBet, WantedBet2 is Money; WantedBet < Money, WantedBet2 is WantedBet; Money = WantedBet, WantedBet2 is WantedBet).

%determines the winner (makes use of modified code from sample code)
%Pass and NewPass are used to indicate whether the players both have enough money to keep on playing after this predicate finishes
get_winner(Out,Print,Pass,Loser,Player1,Player2,PR1,PR2,P1Money,P2Money,Pot,Wins,NP1Money,NP2Money,NewWins,NewPass) :-
	(Loser = both,
	 Pass = yes, NP1Money is P1Money, NP2Money is P2Money, update_wins(tie,Wins,NewWins), print_tie(Out,Print);
	Loser \= both,
	 (Pass = no,
            (Loser = p1, Winner = Player2;
             Loser = p2, Winner = Player1);
	 Pass = yes,
         beats(PR1, PR2, Verdict),
	 (Verdict = PR1, Winner = Player1;
	 Verdict = PR2, Winner = Player2;
	 Verdict = tie, tiebreak(PR1, Player1, Player2, SortedWinner),
	 (SortedWinner = left, Winner = Player1 ;
	 SortedWinner = right, Winner = Player2))),
         (Winner = Player1, Won = "Player One", NP1Money is P1Money + Pot, NP2Money = P2Money;
          Winner = Player2, Won = "Player Two", NP2Money is P2Money + Pot, NP1Money = P1Money),
	 print_winner(Out,Print,Won,Winner,NP1Money,NP2Money),
	 update_wins(Won, Wins, NewWins)),
         (NP1Money < 11, NP2Money >= 11, NewPass = no; NP1Money >= 11, NP2Money < 11, NewPass = no;
          NP1Money >= 11, NP2Money >= 11, NewPass = yes).

update_wins("Player One", [P1Wins,P2Wins,T], [NP1Wins,P2Wins,T]) :- NP1Wins is P1Wins+1.
update_wins("Player Two", [P1Wins,P2Wins,T], [P1Wins,NP2Wins,T]) :- NP2Wins is P2Wins+1.
update_wins(tie, [P1,P2,Ties], [P1,P2,NewTies]) :- NewTies is Ties+1.

print_hands(Out,yes,Hand1,Hand2,Hand1,Hand2) :-
     write("Player 1: "), print(Hand1), nl,
     write("Player 2: "), print(Hand2), nl,
     %%IN THE FILE AS WELL
     write(Out,"Player 1: "), write(Out,Hand1), nl(Out),
     write(Out,"Player 2: "), write(Out,Hand2), nl(Out).
print_hands(Out,no,H1,H2,H1,H2) :-
     write(Out,"Player 1: "), write(Out,H1), nl(Out),
     write(Out,"Player 2: "), write(Out,H2), nl(Out).


print_ranks(Out,yes,P1Rank,P2Rank,P1Rank,P2Rank) :-
	write("Player 1 Hand Rank: "), print(P1Rank), nl,
	write("Player 2 Hand Rank: "), print(P2Rank), nl,
	%%IN THE FILE AS WELL
	write(Out,"Player 1 Hand Rank: "), write(Out,P1Rank), nl(Out),
	write(Out,"Player 2 Hand Rank: "), write(Out,P2Rank), nl(Out).
print_ranks(Out,no,P1,P2,P1,P2) :-
        write(Out,"Player 1 Hand Rank: "), write(Out,P1), nl(Out),
	write(Out,"Player 2 Hand Rank: "), write(Out,P2), nl(Out).

print_bets(Out,yes, Loser, FinalBet,Loser,FinalBet) :-
     write("The determined bet for this round is $"), print(FinalBet), nl,
     (Loser = p1, write("Player 1 cannot make the bet of $"),
      print(FinalBet), nl;
      Loser = p2, write("Player 2 cannot make the bet of $"),
      print(FinalBet), nl;
      Loser = none, write("Player 1 and Player 2 have made the bet of $"),
      print(FinalBet), nl;
      Loser = both, write("Player 1 and Player 2 both fold."), nl),
     %%IN THE FILE AS WELL
     write(Out,"The determined bet for this round is $"),
      write(Out,FinalBet), nl(Out),
     (Loser = p1, write(Out,"Player 1 cannot make the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = p2, write(Out,"Player 2 cannot make the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = none, write(Out,"Player 1 and Player 2 have made the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = both, write(Out,"Player 1 and Player 2 both fold."), nl(Out)).
print_bets(Out,no,Loser,FinalBet,Loser,FinalBet) :-
      write(Out,"The determined bet for this round is $"),
      write(Out,FinalBet), nl(Out),
     (Loser = p1, write(Out,"Player 1 cannot make the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = p2, write(Out,"Player 2 cannot make the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = none, write(Out,"Player 1 and Player 2 have made the bet of $"),
      write(Out,FinalBet), nl(Out);
      Loser = both, write(Out,"Player 1 and Player 2 both fold."), nl(Out)).

print_tie(Out,yes) :- write("This round is a tie. Pot goes to the house."), nl, write("------------------------"), nl,
	%%TO THE FILE TOO
	write(Out,"This round is a tie. Pot goes to the house."), nl(Out), nl(Out), write(Out,"------------------------"), nl(Out).
print_tie(Out,no) :- write(Out,"This round is a tie. Pot of $20 goes to the house."), nl(Out), write(Out,"------------------------"), nl(Out).

print_winner(Out,yes, Winner, WinnerHand, NP1Money, NP2Money) :-
	write("Winner: "), write(Winner), write(" with hand: "),
	print(WinnerHand), nl, write("Player 1 Money: $"), print(NP1Money), nl,
	write("Player 2 Money: $"), print(NP2Money), nl, write("------------------------"), nl,
	%%TO THE FILE TOO
	write(Out,"Winner: "), write(Out,Winner), write(Out," with hand: "),
	write(Out,WinnerHand), nl(Out), write(Out,"Player 1 Money: $"), write(Out,NP1Money), nl(Out),
	write(Out,"Player 2 Money: $"), write(Out,NP2Money),
	nl(Out), write(Out,"------------------------"), nl(Out).
print_winner(Out,no,Winner,WinnerHand,NP1Money,NP2Money) :-
	write(Out,"Winner: "), write(Out,Winner), write(Out," with hand: "),
	write(Out,WinnerHand), nl(Out), write(Out,"Player 1 Money: $"),
	write(Out,NP1Money), nl(Out),
	write(Out,"Player 2 Money: $"), write(Out,NP2Money),
	nl(Out), write(Out,"------------------------"), nl(Out).


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
determine_hand([[A,X],[B,X],[C,X]],straight_flush) :-
   successor(C,B), successor(B,A).
%if the three ranks are the same, three of a kind
determine_hand([[A,_],[B,_],[C,_]],three_of_a_kind) :- A = B, A = C.
%if the suits are all the same, flush
determine_hand([[_,X],[_,Y],[_,Z]],flush) :- X = Y, X = Z.
%if the ranks are in order, straight
determine_hand([[A,_],[B,_],[C,_]],straight) :-
  successor(C,B), successor(B,A).
%if two of the ranks match, pair
determine_hand([[A,_],[B,_],[C,_]],pair) :-
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

?- time(poker(100)).
?- time(poker(500)).
?- time(poker(1000)).
