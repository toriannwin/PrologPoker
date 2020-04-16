# PrologPoker
Multiple Variants of 3-Card-Poker Related Prolog Programs
A3card1: The basic generation of a deck of cards, shuffling, drawing 3 cards to determine the rank of the hand. Calculates frequency of every hand rank. Run: poker(N) where N = number of "games"
A3card2: Two players both getting dealt 3 cards, determining the rank, and then the winner. Also calculates relevent statistics. Run: poker(N) where N = number of games
A3card3: Two players, where a player makes a guess as to whether it will win based on statistics gathered from A3card2. Precision rate of these guesses is calculated. Run: poker(N) where N = number of games
A3card4: Two players, where each player must bet 10 units of money to start a round, and 1 unit of money (or 0 if they fold) once they have their hand. Run: poker(M) where M = starting money
A3card5: Four players, implemented with a dealer that changes with each game, so that betting happens in order based on who was the dealer. Bets must match or up the previous bet, otherwise the player must back out. Run: poker(M) where M = starting money
