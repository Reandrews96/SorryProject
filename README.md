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

<h1> Instructions </h1>
<newline>
<newline>
To fire this up, you should open up a new buffer in ACLEMACS within the folder that our project
  is in. To do this, type <b>aclemacs</b> into the terminal. Then <b>ESC + X</b>, then type <b>fi:common-lisp</b> and <b>ENTER</b> seven times to open up lisp in emacs.

In order to run our specific project, type **(load "basic-defns")**, and run **(maker)** which will allow you to compile and load all of the relevant files for our implementation of sorry.

Type **(setf g (make-sorry))** to create a new game.

<h2> To play manually against yourself </h2>

Type **(draw-card g)** to draw a card to start your turn.
Type **(play-card g index &optional secondary)** to play your card on a particular piece (specify the piece using index). 
When the card is the Sorry, the index is used to specify which piece belonging to the other player that you want to send to start.
You can use the optional index to specify the secondary use of the card if you have a 10 and you want to use it as a -1. To do this, just type -1 in the parameter spot for secondary.
Type **(pass g)** to pass if you have no available moves.

**Important note**: When you specify an index to apply a card to, if you select a piece that is at the start, the piece at start with the lowest number will be selected (none of these are distinguishable anyway). This is only relevant when applying a 1 to a piece at the start.

<h3> Get some help playing your move </h3>

Type **(suggest-best-move g depth &optional eval-choice)** with a depth value (can do any depth value 8 or below, but 6 and 8 can be a little slow) and an optional numeric value for your choice of evaluation function. If no value is specified, the default is used. 1 indicates the offensive strategy, 2 the defensive, and 3 the runner. This will print out the best move and how to use it above the current game using the play-card function.

Type **(do-best-move g depth &optional eval-choice)** in order to just do the best move that would have been suggested in the same way as using suggest-best-move.

<h4>Play Against the AI</h4>

You can select your own moves manually, but for the other player, you will always have them use <b>(do-best-move g depth &optional eval-choice)</b> as shown above.

<h2> Competing at multiple depths or with multiple evaluation functions </h2>

For playing with another player, type **(compete red-depth green-depth g)** in which <i> red-depth </i> is the depth at which
you want the search to go for the red player, <i>green-depth</i> is the same but for the green player, and <i>g</i> is your 
Sorry! game.

If you want to play against a random player, use **(compete-vs-random depth g)** in which <i>depth</i> is the depth at which
you want to conclude the search and <i>g</i> is your Sorry! game. 

If you want to play two evaluation functions against each other, use **(compete-diff-eval eval-red eval-green g)** in which <i>eval-red</i> is the evaluation function used by red, <i>eval-green</i> is the evaluation function used by green and <i>g</i> is your Sorry! game. 


This should print out the <b> Root Node Value </b>, <b>Number of Moves done</b>, <b> Number of Nodes Pruned </b>, 
<b> Best Move</b>, and <b>Card Chosen</b>.
