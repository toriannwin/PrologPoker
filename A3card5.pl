%Tori Windrich - taw170030 - Assignment 3 Task 5

print_results([P1M,P2M,P3M,P4M],[P1,P2,P3,P4,Ties]) :-
    write("Player One won "), print(P1), write(" times."), nl,
    write("Player Two won "), print(P2), write(" times."), nl,
    write("Player Three won "), print(P3), write(" times."), nl,
    write("Player Four won "), print(P4), write(" times."), nl,
    write("Games ended in a tie "), print(Ties), write(" times."), nl,
    write("Player One ended with $"), print(P1M), write("."), nl,
    write("Player Two ended with $"), print(P2M), write("."), nl,
    write("Player Three ended with $"), print(P3M), write("."), nl,
    write("Player Four ended with $"), print(P4M), write("."), nl,
    Moneys = [P1M,P2M,P3M,P4],
    max_member(X,Moneys),
    (X = P1M, write("Player One Wins!!"), nl;
     X = P2M, write("Player Two Wins!!"), nl;
     X = P3M, write("Player Three Wins!!"), nl;
     X = P4M, write("Player Four Wins!!"), nl).

% For the passed in integer N, run N games, and then print out the
% resulting hand ranks.
poker(M) :-
	atom_concat(a3card5out,M,Filename),
	string_concat(Filename,'.txt',File),
	open(File,write,Out),
	write("BEGINNING GAMES, BOTH PLAYERS STARTING WITH $"),
        print(M), write("."), nl,
	poker(Out,[M,M,M,M],0,[0,0,0,0,0],Moneys,WinResults,yes,p1),
        write("GAMES COMPLETE. RESULTS:"), nl,
	print_results(Moneys,WinResults),
	close(Out).

%handles the looping of the games
%Pass indicates if the players have enough money to move on to another game
%Wins keeps track of the number of wins and ties of each player.
poker(_,Moneys,100,Wins,Moneys,Wins,_,_). %If N is greater than 99, return all current results
poker(_,Moneys,_,Wins,Moneys,Wins,no,_).
poker(Out,Moneys,N,Wins,X,Z,Pass,Turn) :- %if N is not zero, we take in the current Results, and we don't care what X is
   % P1Money > 10, P2Money > 10,
    get_deck(Deck),
    initial_bets(Moneys,BaseMoneys,Pass),
    get_print(BaseMoneys,N,Print),
    (Pass = yes, game(Out,Print,Deck,BaseMoneys,Wins,NewMoneys,NewWins,NewPass,Turn);
     Pass = no, NewMoneys = Moneys, NewWins = Wins),
    N2 is N+1, %increase N by one, store in N2
    (Turn = p4, NewTurn = p1;
     Turn = p3, NewTurn = p4;
     Turn = p2, NewTurn = p3;
     Turn = p1, NewTurn = p2),
    poker(Out,NewMoneys,N2,NewWins,X,Z,NewPass,NewTurn). %call Poker again with N+1

%call the essential game predicates
game(Out,Print,Deck,Moneys,Wins,NewMoneys,NewWins,Pass,Turn) :-
    deal_hands(Out,Print,Deck,Turn,PlayerHands),
    determine_all_hands(Out,Print,PlayerHands,PlayerResults),
    do_bets(Out,Print,PlayerResults,Moneys,PlayerMoneys,Turn,FirstPass,Remaining,Pot),
    get_winner(Out,Print,FirstPass,Remaining,PlayerHands,PlayerResults,PlayerMoneys,Pot,Wins,NewMoneys,NewWins,Pass).

%Deal the top card from the passed in Deck
deal_12_cards([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12|Deck],[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],Deck).

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
initial_bets([P1,P2,P3,P4],[NP1,NP2,NP3,NP4],Pass) :-
    (P1 >= 10, NP1 is P1 - 10, Pass1 = yes;
     P1 < 10, NP1 is P1, Pass1 = no),
    (P2 >= 10, NP2 is P2 - 10, Pass2 = yes;
     P2 < 10, NP2 is P2, Pass2 = no),
    (P3 >= 10, NP3 is P3 - 10, Pass3 = yes;
     P3 < 10, NP3 is P3, Pass3 = no),
    (P4 >= 10, NP4 is P4 - 10, Pass4 = yes;
     P4 < 10, NP4 is P4, Pass4 = no),
    Passes = [Pass1,Pass2,Pass3,Pass4],
    (member(no,Passes), Pass = no; not(member(no,Passes)), Pass = yes).

%determine if it's time to print out the output
get_print(_,0,yes).
get_print(_,99,yes).
get_print([P1,P2,P3,P4],_,Print) :- (P1 < 11, Print1 = yes; P1 >= 11, Print1 = no),
                                    (P2 < 11, Print2 = yes; P2 >= 11, Print2 = no),
                                    (P3 < 11, Print3 = yes; P3 >= 11, Print3 = no),
                                    (P4 < 11, Print4 = yes; P4 >= 11, Print4 = no),
                                    Prints = [Print1, Print2, Print3, Print4],
                                    (member(yes,Prints), Print = yes; not(member(yes,Prints)), Print = no).

%Deal hands for both players
deal_hands(Out,Print,Deck,Turn,Players) :-
    deal(Deck,Turn,[PlayerOne,PlayerTwo,PlayerThree,PlayerFour]),
    sort_hand(PlayerOne,SortedPlayer1), %sort the Hand to make it easier to determine hand ranks
    sort_hand(PlayerTwo,SortedPlayer2),
    sort_hand(PlayerThree,SortedPlayer3), %sort the Hand to make it easier to determine hand ranks
    sort_hand(PlayerFour,SortedPlayer4),
    Players = [SortedPlayer1,SortedPlayer2,SortedPlayer3,SortedPlayer4],
    print_hands(Out,Print,Players).

deal(Deck,p1,[P1,P2,P3,P4]) :-
    deal_12_cards(Deck,[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],_),
    P2 = [C1,C5,C9],
    P3 = [C2,C6,C10],
    P4 = [C3,C7,C11],
    P1 = [C4,C8,C12].
deal(Deck,p2,[P1,P2,P3,P4]) :-
    deal_12_cards(Deck,[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],_),
    P3 = [C1,C5,C9],
    P4 = [C2,C6,C10],
    P1 = [C3,C7,C11],
    P2 = [C4,C8,C12].
deal(Deck,p3,[P1,P2,P3,P4]) :-
    deal_12_cards(Deck,[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],_),
    P4 = [C1,C5,C9],
    P1 = [C2,C6,C10],
    P2 = [C3,C7,C11],
    P3 = [C4,C8,C12].
deal(Deck,p4,[P1,P2,P3,P4]) :-
    deal_12_cards(Deck,[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],_),
    P1 = [C1,C5,C9],
    P2 = [C2,C6,C10],
    P3 = [C3,C7,C11],
    P4 = [C4,C8,C12].

%determine the ranks of the hands of both players
determine_all_hands(Out,Print,[P1,P2,P3,P4],[P1R,P2R,P3R,P4R]) :-
    determine_hand(P1,P1R),
    determine_hand(P2,P2R),
    determine_hand(P3,P3R),
    determine_hand(P4,P4R),
    print_ranks(Out,Print,[P1R,P2R,P3R,P4R]).

%get the betting amount for this game
do_bets(Out,Print,Ranks,[M1,M2,M3,M4],[NM1,NM2,NM3,NM4],Turn,Pass,Remaining,Pot) :-
    get_bets(Turn,Ranks,[M1,M2,M3,M4],[FB1,FB2,FB3,FB4],Bluffed),
    Pot is FB1 + FB2 + FB3 + FB4 + 4*10,
    (FB1 \= 0, append([],[p1],R1); FB1 = 0, append([],[],R1)),
    (FB2 \= 0, append(R1,[p2],R2); FB2 = 0, append(R1,[],R2)),
    (FB3 \= 0, append(R2,[p3],R3); FB3 = 0, append(R2,[],R3)),
    (FB4 \= 0, append(R3,[p4],Remaining); FB4 = 0, append(R3,[],Remaining)),
    NM1 is M1 - FB1,
    NM2 is M2 - FB2,
    NM3 is M3 - FB3,
    NM4 is M4 - FB4,
    length(Remaining,Len),
    (Len < 2, Pass = no; Len >= 2, Pass = yes),
    print_bets(Out,Print,Turn,[FB1,FB2,FB3,FB4],Bluffed,Remaining,Pot).


get_bets(p1,[R1,R2,R3,R4],[M1,M2,M3,M4],[FB1,FB2,FB3,FB4],[B1,B2,B3,B4]) :-
    bet(M2,R2,FB2,B2),
    bet(M3,FB2,R3,FB3,B3),
    (FB3 = 0, Nxt1 is FB2; FB3 \= 0, Nxt1 is FB3),
    bet(M4,Nxt1,R4,FB4,B4),
    (FB4 = 0, Nxt2 is Nxt1; FB4 \= 0, Nxt2 is FB4),
    bet(M1,Nxt2,R1,FB1,B1).
get_bets(p2,[R1,R2,R3,R4],[M1,M2,M3,M4],[FB1,FB2,FB3,FB4],[B1,B2,B3,B4]) :-
    bet(M3,R3,FB3,B3),
    bet(M4,FB3,R4,FB4,B4),
    (FB4 = 0, Nxt1 is FB3; FB4 \= 0, Nxt1 is FB4),
    bet(M1,Nxt1,R1,FB1,B1),
    (FB1 = 0, Nxt2 is Nxt1; FB1 \= 0, Nxt2 is FB1),
    bet(M2,Nxt2,R2,FB2,B2).
get_bets(p3,[R1,R2,R3,R4],[M1,M2,M3,M4],[FB1,FB2,FB3,FB4],[B1,B2,B3,B4]) :-
    bet(M4,R4,FB4,B4),
    bet(M1,FB4,R1,FB1,B1),
    (FB1 = 0, Nxt1 is FB4; FB1 \= 0, Nxt1 is FB1),
    bet(M2,Nxt1,R2,FB2,B2),
    (FB2 = 0, Nxt2 is Nxt1; FB2 \= 0, Nxt2 is FB2),
    bet(M3,Nxt2,R3,FB3,B3).
get_bets(p4,[R1,R2,R3,R4],[M1,M2,M3,M4],[FB1,FB2,FB3,FB4],[B1,B2,B3,B4]) :-
    bet(M1,R1,FB1,B1),
    bet(M2,FB1,R2,FB2,B2),
    (FB2 = 0, Nxt1 is FB1; FB2 \= 0, Nxt1 is FB2),
    bet(M3,Nxt1,R3,FB3,B3),
    (FB3 = 0, Nxt2 is Nxt1; FB3 \= 0, Nxt2 is FB3),
    bet(M4,Nxt2,R4,FB4,B4).


%uses prediction to then determine how much should be bet based on the hand
bet(Money,PreviousBet,Rank,EndBet,Bluffed) :-
    random_between(0,100,Bluff),
    (Bluff >= 95, Money >= 10, random_between(7,10,WB), Bluffed = yes;
     Bluff >= 95, Money < 10, Bluffed = no;
     Bluff < 95, Money > 0, Bluffed = no),
    (Bluffed = no, get_prediction(Rank,Prediction), get_base_bet(Rank,Prediction,WantedBet);
     Bluffed = yes, WantedBet is WB),
    %deal with possibly the bet being too high for the amount of money the player has
   (Money < WantedBet, WantedBet2 is Money; WantedBet < Money, WantedBet2 is WantedBet; Money = WantedBet, WantedBet2 is WantedBet),
   %deal with the possibility that the player may not want to stay in the game
   Difference is PreviousBet - WantedBet2,
   (Difference > 5, EndBet is 0; %if the previous bet is much higher, choose to drop out
    Difference < 6,
   %deal with possibility of the bet being too low to match the previous bet
   (Money < PreviousBet, PreviousBet \= 0, WantedBet2 >= 0, EndBet is 0; %can't match, drop out
    Money >= PreviousBet, PreviousBet \= 0, WantedBet2 < PreviousBet, EndBet is PreviousBet; %match
    Money >= PreviousBet, PreviousBet \= 0, WantedBet2 >= PreviousBet, EndBet is WantedBet2;
    Money >= PreviousBet, PreviousBet = 0, WantedBet2 = PreviousBet, EndBet is 1)).
bet(Money,Rank,EndBet,Bluffed) :-
    random_between(0,100,Bluff),
    (Bluff >= 95, Money >= 10, random_between(7,10,WB), Bluffed = yes;
     Bluff >= 95, Money < 10, Bluffed = no;
     Bluff < 95, Money > 0, Bluffed = no),
    (Bluffed = no, get_prediction(Rank,Prediction), get_base_bet(Rank,Prediction,WantedBet1);
     Bluffed = yes, WantedBet1 is WB),
    (WantedBet1 = 0, WantedBet is 1; WantedBet1 \= 0, WantedBet is WantedBet1),
   (Money < WantedBet, EndBet is Money; WantedBet < Money, EndBet is WantedBet; Money = WantedBet, EndBet is WantedBet).


get_prediction(Rank,Prediction) :-
   (Rank = straight_flush, Prediction = win;
    Rank = three_of_a_kind, Prediction = win;
    Rank = straight, straight_chance(Prediction);
    Rank = flush, flush_chance(Prediction);
    Rank = pair, pair_chance(Prediction);
    Rank = high_card, high_card_chance(Prediction)).
get_base_bet(Rank,Prediction,WantedBet) :-
   (Rank = straight_flush, Prediction = win, WantedBet is 10;
    Rank = three_of_a_kind, Prediction = win, WantedBet is 9;
    Rank = straight, Prediction = win, WantedBet is 8;
    Rank = straight, Prediction = lose, WantedBet is 6;
    Rank = flush, Prediction = win, WantedBet is 7;
    Rank = flush, Prediction = lose, WantedBet is 5;
    Rank = pair, Prediction = win, WantedBet is 4;
    Rank = pair, Prediction = lose, WantedBet is 2;
    Rank = high_card, Prediction = win, WantedBet is 1;
    Rank = high_card, Prediction = lose, WantedBet is 0).

%determines the winner (makes use of modified code from sample code)
%Pass and NewPass are used to indicate whether the players both have enough money to keep on playing after this predicate finishes
get_winner([p1],[P1,_,_,_],P1).
get_winner([p2],[_,P2,_,_],P2).
get_winner([p3],[_,_,P3,_],P3).
get_winner([p4],[_,_,_,P4],P4).
get_winner([p1,p2],[P1,P2,_,_],[PR1,PR2,_,_],Winner) :- find_winner(P1,P2,PR1,PR2,Winner,_).
get_winner([p1,p3],[P1,_,P3,_],[PR1,_,PR3,_],Winner) :- find_winner(P1,P3,PR1,PR3,Winner,_).
get_winner([p1,p4],[P1,_,_,P4],[PR1,_,_,PR4],Winner) :- find_winner(P1,P4,PR1,PR4,Winner,_).
get_winner([p2,p3],[_,P2,P3,_],[_,PR2,PR3,_],Winner) :- find_winner(P2,P3,PR2,PR3,Winner,_).
get_winner([p2,p4],[_,P2,_,P4],[_,PR2,_,PR4],Winner) :- find_winner(P2,P4,PR2,PR4,Winner,_).
get_winner([p3,p4],[_,_,P3,P4],[_,_,PR3,PR4],Winner) :- find_winner(P3,P4,PR3,PR4,Winner,_).
get_winner([p1,p2,p3],[P1,P2,P3,_],[PR1,PR2,PR3,_],Winner) :-
    find_winner(P1,P2,PR1,PR2,Winner1,WinRank1),
    find_winner(Winner1,P3,WinRank1,PR3,Winner,_).
get_winner([p1,p3,p4],[P1,_,P3,P4],[PR1,_,PR3,PR4],Winner) :-
    find_winner(P1,P3,PR1,PR3,Winner1,WinRank1),
    find_winner(Winner1,P4,WinRank1,PR4,Winner,_).
get_winner([p1,p2,p4],[P1,P2,_,P4],[PR1,PR2,_,PR4],Winner) :-
    find_winner(P1,P2,PR1,PR2,Winner1,WinRank1),
    find_winner(Winner1,P4,WinRank1,PR4,Winner,_).
get_winner([p2,p3,p4],[_,P2,P3,P4],[_,PR2,PR3,PR4],Winner) :-
    find_winner(P2,P3,PR2,PR3,Winner1,WinRank1),
    find_winner(Winner1,P4,WinRank1,PR4,Winner,_).
get_winner([p1,p2,p3,p4],[P1,P2,P3,P4],[PR1,PR2,PR3,PR4],Winner) :-
    find_winner(P1,P2,PR1,PR2,Win1,Rank1),
    find_winner(P3,P4,PR3,PR4,Win2,Rank2),
    find_winner(Win1,Win2,Rank1,Rank2,Winner,_).

find_winner(P1,P2,PR1,PR2,Winner,WinRank) :-
    beats(PR1, PR2, Verdict),
	 (Verdict = PR1, Winner = P1;
	 Verdict = PR2, Winner = P2;
	 Verdict = tie, tiebreak(PR1, P1, P2, SortedWinner),
	 (SortedWinner = left, Winner = P1 ;
	 SortedWinner = right, Winner = P2)),
         (Winner = P1, WinRank = PR1; Winner = P2, WinRank = PR2).

get_winner(Out,Print,Pass,Remaining,[P1,P2,P3,P4],Ranks,[P1M,P2M,P3M,P4M],Pot,Wins,[NM1,NM2,NM3,NM4],NewWins,NewPass) :-
        length(Remaining,Num),
	 (Pass = no, Num = 0, Winner = tie;
          Pass = no, Num = 1, get_winner(Remaining,[P1,P2,P3,P4],Winner);
	  Pass = yes, Num > 1, get_winner(Remaining,[P1,P2,P3,P4],Ranks,Winner)),
         (Winner = P1, Won = "Player One", NM1 is P1M + Pot, NM2 = P2M, NM3 = P3M, NM4 = P4M;
          Winner = P2, Won = "Player Two", NM2 is P2M + Pot, NM1 = P1M, NM3 = P3M, NM4 = P4M;
          Winner = P3, Won = "Player Three", NM3 is P3M + Pot, NM2 = P2M, NM1 = P1M, NM4 = P4M;
          Winner = P4, Won = "Player Four", NM4 is P4M + Pot, NM1 = P1M, NM3 = P3M, NM2 = P2M;
          Winner = tie, Won = tie, NM1 is P1M, NM2 is P2M, NM3 is P3M, NM4 is P3M),
	 print_winner(Out,Print,Won,Winner,[NM1,NM2,NM3,NM4]),
	 update_wins(Won, Wins, NewWins),
         (NM1 < 10, Pass1 = no; NM1 >= 10, Pass1 = yes),
         (NM2 < 10, Pass2 = no; NM2 >= 10, Pass2 = yes),
         (NM3 < 10, Pass3 = no; NM3 >= 10, Pass3 = yes),
         (NM4 < 10, Pass4 = no; NM4 >= 10, Pass4 = yes),
         Passes = [Pass1,Pass2,Pass3,Pass4],
         (member(no,Passes), NewPass = no; not(member(no,Passes)), NewPass = yes).

update_wins("Player One", [P1Wins,P2Wins,P3Wins,P4Wins,T], [NP1Wins,P2Wins,P3Wins,P4Wins,T]) :- NP1Wins is P1Wins+1.
update_wins("Player Two", [P1Wins,P2Wins,P3Wins,P4Wins,T], [P1Wins,NP2Wins,P3Wins,P4Wins,T]) :- NP2Wins is P2Wins+1.
update_wins("Player Three", [P1Wins,P2Wins,P3Wins,P4Wins,T], [P1Wins,P2Wins,NP3Wins,P4Wins,T]) :- NP3Wins is P3Wins+1.
update_wins("Player Four", [P1Wins,P2Wins,P3Wins,P4Wins,T], [P1Wins,P2Wins,P3Wins,NP4Wins,T]) :- NP4Wins is P4Wins+1.
update_wins(tie, [P1,P2,P3,P4,Ties], [P1,P2,P3,P4,NewTies]) :- NewTies is Ties+1.


print_hands(Out,yes,[H1,H2,H3,H4]) :-
     write("Player 1: "), print(H1), nl,
     write("Player 2: "), print(H2), nl,
     write("Player 3: "), print(H3), nl,
     write("Player 4: "), print(H4), nl,
     %%IN THE FILE AS WELL
     write(Out,"Player 1: "), write(Out,H1), nl(Out),
     write(Out,"Player 2: "), write(Out,H2), nl(Out),
     write(Out,"Player 3: "), write(Out,H3), nl(Out),
     write(Out,"Player 4: "), write(Out,H4), nl(Out).
print_hands(Out,no,[H1,H2,H3,H4]) :-
     write(Out,"Player 1: "), write(Out,H1), nl(Out),
     write(Out,"Player 2: "), write(Out,H2), nl(Out),
     write(Out,"Player 3: "), write(Out,H3), nl(Out),
     write(Out,"Player 4: "), write(Out,H4), nl(Out).

print_ranks(Out,yes,[R1,R2,R3,R4]) :-
	write("Player 1 Hand Rank: "), print(R1), nl,
	write("Player 2 Hand Rank: "), print(R2), nl,
        write("Player 3 Hand Rank: "), print(R3), nl,
	write("Player 4 Hand Rank: "), print(R4), nl,
	%%IN THE FILE AS WELL
	write(Out,"Player 1 Hand Rank: "), write(Out,R1), nl(Out),
	write(Out,"Player 2 Hand Rank: "), write(Out,R2), nl(Out),
        write(Out,"Player 3 Hand Rank: "), write(Out,R3), nl(Out),
	write(Out,"Player 4 Hand Rank: "), write(Out,R4), nl(Out).
print_ranks(Out,no,[R1,R2,R3,R4]) :-
        write(Out,"Player 1 Hand Rank: "), write(Out,R1), nl(Out),
	write(Out,"Player 2 Hand Rank: "), write(Out,R2), nl(Out),
        write(Out,"Player 3 Hand Rank: "), write(Out,R3), nl(Out),
	write(Out,"Player 4 Hand Rank: "), write(Out,R4), nl(Out).

print_winner(Out,yes,tie,_,[M1,M2,M3,M4]) :-
    write("There was a tie. Pot goes to the house. The Players Moneys are:\n\tPlayer 1: $"), write(M1), write("\n\tPlayer 2: $"), write(M2), write("\n\tPlayer 3: $"), write(M3), write("\n\tPlayer 4: $"), write(M4), nl,
    %TO OUTPUT FILE
    write(Out,"There was a tie. Pot goes to the house. The Players Moneys are:\n\tPlayer 1: $"), write(Out,M1), write(Out,"\n\tPlayer 2: $"), write(Out,M2), write(Out,"\n\tPlayer 3: $"), write(Out,M3), write(Out,"\n\tPlayer 4: $"), write(Out,M4), nl(Out).
print_winner(Out,no,tie,_,[M1,M2,M3,M4]) :-
    write(Out,"There was a tie. Pot goes to the house. The Players Moneys are:\n\tPlayer 1: $"), write(Out,M1), write(Out,"\n\tPlayer 2: $"), write(Out,M2), write(Out,"\n\tPlayer 3: $"), write(Out,M3), write(Out,"\n\tPlayer 4: $"), write(Out,M4), nl(Out).
print_winner(Out,yes, Winner, WinnerHand, [M1,M2,M3,M4]) :-
	write("Winner: "), write(Winner), write(" with hand: "),
	print(WinnerHand), nl, write("The Players Moneys are:\n\tPlayer One: $"), write(M1), write("\n\tPlayer 2: $"), write(M2), write("\n\tPlayer 3: $"), write(M3), write("\n\tPlayer 4: $"), write(M4), nl, write("------------------------"), nl,
	%%TO THE FILE TOO
	write(Out,"Winner: "), write(Out,Winner), write(Out," with hand: "),
	write(Out,WinnerHand), nl(Out), write(Out,"The Players Moneys are:\n\tPlayer 1: $"), write(Out,M1), write(Out,"\n\tPlayer 2: $"), write(Out,M2), write(Out,"\n\tPlayer 3: $"), write(Out,M3), write(Out,"\n\tPlayer 4: $"), write(Out,M4),
	nl(Out), write(Out,"------------------------"), nl(Out).
print_winner(Out,no,Winner,WinnerHand,[M1,M2,M3,M4]) :-
    write(Out,"Winner: "), write(Out,Winner), write(Out," with hand: "),
	write(Out,WinnerHand), nl(Out), write(Out,"The Players Moneys are:\n\tPlayer 1: $"), write(Out,M1), write(Out,"\n\tPlayer 2: $"), write(Out,M2), write(Out,"\n\tPlayer 3: $"), write(Out,M3), write(Out,"\n\tPlayer 4: $"), write(Out,M4),
	nl(Out), write(Out,"------------------------"), nl(Out).

%Print = yes, print to the console and the file
print_bets(Out,yes,Turn,[FB1,FB2,FB3,FB4],[B1,B2,B3,B4],Remaining,Pot) :-
   (Turn = p1, Dealer = "Player 1", FirstBet = "Player 2"; Turn = p2, Dealer = "Player 2", FirstBet = "Player 3";
    Turn = p3, Dealer = "Player 3", FirstBet = "Player 4"; Turn = p4, Dealer = "Player 4", FirstBet = "Player 1"),
   write("It was "), write(Dealer), write("'s turn to deal this turn. So, the first bet came from "), write(FirstBet), write("."), nl,
   write("The Final Bets Came Out To: "), nl,
   write("Player 1: $"), write(FB1), write(". Did they bluff? "), write(B1), nl,
   write("Player 2: $"), write(FB2), write(". Did they bluff? "), write(B2), nl,
   write("Player 3: $"), write(FB3), write(". Did they bluff? "), write(B3), nl,
   write("Player 4: $"), write(FB4), write(". Did they bluff? "), write(B4), nl,
   write("The Final Pot For This Round Is: $"), write(Pot), nl,
   print_remaining(Remaining),
   %%FOR THE FILE
   (Turn = p1, Dealer = "Player 1", FirstBet = "Player 2"; Turn = p2, Dealer = "Player 2", FirstBet = "Player 3";
    Turn = p3, Dealer = "Player 3", FirstBet = "Player 4"; Turn = p4, Dealer = "Player 4", FirstBet = "Player 1"),
   write(Out,"It was "), write(Out,Dealer), write(Out,"'s turn to deal this turn. So, the first bet came from "), write(Out,FirstBet), write(Out,"."), nl(Out),
   write(Out,"The Final Bets Came Out To: "), nl(Out),
   write(Out,"Player 1: $"), write(Out,FB1), write(Out,". Did they bluff? "), write(Out,B1), nl(Out),
   write(Out,"Player 2: $"), write(Out,FB2), write(Out,". Did they bluff? "), write(Out,B2), nl(Out),
   write(Out,"Player 3: $"), write(Out,FB3), write(Out,". Did they bluff? "), write(Out,B3), nl(Out),
   write(Out,"Player 4: $"), write(Out,FB4), write(Out,". Did they bluff? "), write(Out,B4), nl(Out),
   write(Out,"The Final Pot For This Round Is: $"), write(Out,Pot), nl(Out),
   print_remaining(Out,Remaining).
%Print = no, print only to the file
print_bets(Out,no,Turn,[FB1,FB2,FB3,FB4],[B1,B2,B3,B4],Remaining,Pot) :-
    (Turn = p1, Dealer = "Player 1", FirstBet = "Player 2"; Turn = p2, Dealer = "Player 2", FirstBet = "Player 3";
    Turn = p3, Dealer = "Player 3", FirstBet = "Player 4"; Turn = p4, Dealer = "Player 4", FirstBet = "Player 1"),
   write(Out,"It was "), write(Out,Dealer), write(Out,"'s turn to deal this turn. So, the first bet came from "), write(Out,FirstBet), write(Out,"."), nl(Out),
   write(Out,"The Final Bets Came Out To: "), nl(Out),
   write(Out,"Player 1: $"), write(Out,FB1), write(Out,". Did they bluff? "), write(Out,B1), nl(Out),
   write(Out,"Player 2: $"), write(Out,FB2), write(Out,". Did they bluff? "), write(Out,B2), nl(Out),
   write(Out,"Player 3: $"), write(Out,FB3), write(Out,". Did they bluff? "), write(Out,B3), nl(Out),
   write(Out,"Player 4: $"), write(Out,FB4), write(Out,". Did they bluff? "), write(Out,B4), nl(Out),
   write(Out,"The Final Pot For This Round Is: $"), write(Out,Pot), nl(Out),
   print_remaining(Out,Remaining).

print_remaining(List) :- write("Remaining Players: "),
    (member(p1,List), write("Player 1, "); not(member(p1,List)), write("")),
    (member(p2,List), write("Player 2, "); not(member(p2,List)), write("")),
    (member(p3,List), write("Player 3, "); not(member(p3,List)), write("")),
    (member(p4,List), write("Player 4."),nl; not(member(p4,List)), write(""),nl).
print_remaining(Out,List) :- write(Out,"Remaining Players: "),
    (member(p1,List), write(Out,"Player 1, "); not(member(p1,List)),write("")),
    (member(p2,List), write(Out,"Player 2, "); not(member(p2,List)),write("")),
    (member(p3,List), write(Out,"Player 3, "); not(member(p3,List)),write("")),
    (member(p4,List), write(Out,"Player 4."), nl(Out); not(member(p4,List)),write(""), nl(Out)).

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
determine_hand([[A,_],[A,_],[A,_]],three_of_a_kind).
%if the suits are all the same, flush
determine_hand([[_,X],[_,X],[_,X]],flush).
%if the ranks are in order, straight
determine_hand([[A,_],[B,_],[C,_]],straight) :-
  successor(C,B), successor(B,A).
%if two of the ranks match, pair
determine_hand([[A,_],[A,_],[_,_]],pair).
determine_hand([[_,_],[A,_],[A,_]],pair).
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
