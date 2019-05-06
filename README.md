# SorryProject
AI class project

The basic premise for this project is to construct a two player version of the Hasbro game “Sorry!” 
in a similar manner to the chess game created by Professor Hunsberger, including the addition of minimax 
and alpha-beta pruning. The game of Sorry! involves moving pieces around a board based on the selected
card, where each card has some probability of being pulled from the deck. For any card, it may be possible
to move any one (or several) of the pieces. The goal of this game is to move all your pieces into the 
“home” space before your opponent, while also have the ability to send opponent pieces back to the starting
space along the way. This game involves chance, so we modified minimax to include that.

You can play either with someone else or against a random computer using our AI that should suggest the best
moves possible. 

<b> Instructions </b>
<newline>
To fire this up, you should open up a new buffer in ACLEMACS within the folder that our project
is in. Then type (load "sorry"), (load "minimax"), (load "eval-func"),
and (load "compete").

Type (setf g (make-sorry)) to create a new game.

To play manually against yourself:
Type (draw-card g) to draw a card to start your turn.
Type (play-card g index &optional optional-index) to play your card on a particular piece (specify the piece using index) and use the optional index to specify the index of an opponents piece if you are using the sorry! card.

**Important note**: When you specify an index to apply a card to, if you select a piece that is at the start, the piece at start with the lowest number will be selected (none of these are distinguishable anyway).

For playing with another player, type (compete red-depth green-depth g) in which <i> red-depth </i> is the depth at which
you want the search to go for the red player, <i>green-depth</i> is the same but for the green player, and <i>g</i> is your 
Sorry! game.

If you want to play against a random player, use (compete-vs-random depth g) in which <i>depth</i> is the depth at which
you want to conclude the search and <i>g</i> is your Sorry! game. 

This should print out the <b> Root Node Value </b>, <b>Number of Moves done</b>, <b> Number of Nodes Pruned </b>, 
<b> Best Move</b>, and <b>Card Choosen</b>.
