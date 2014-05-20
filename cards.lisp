;;;; cards.lisp

(in-package #:cards)

(defconstant *standard-deck*
  (make-array 52 :element-type 'character
		 :initial-contents '(#\PLAYING_CARD_ACE_OF_SPADES
				     #\PLAYING_CARD_TWO_OF_SPADES
				     #\PLAYING_CARD_THREE_OF_SPADES
				     #\PLAYING_CARD_FOUR_OF_SPADES
				     #\PLAYING_CARD_FIVE_OF_SPADES
				     #\PLAYING_CARD_SIX_OF_SPADES
				     #\PLAYING_CARD_SEVEN_OF_SPADES
				     #\PLAYING_CARD_EIGHT_OF_SPADES
				     #\PLAYING_CARD_NINE_OF_SPADES
				     #\PLAYING_CARD_TEN_OF_SPADES
				     #\PLAYING_CARD_JACK_OF_SPADES
				     #\PLAYING_CARD_QUEEN_OF_SPADES
				     #\PLAYING_CARD_KING_OF_SPADES
				     #\PLAYING_CARD_ACE_OF_HEARTS
				     #\PLAYING_CARD_TWO_OF_HEARTS
				     #\PLAYING_CARD_THREE_OF_HEARTS
				     #\PLAYING_CARD_FOUR_OF_HEARTS
				     #\PLAYING_CARD_FIVE_OF_HEARTS
				     #\PLAYING_CARD_SIX_OF_HEARTS
				     #\PLAYING_CARD_SEVEN_OF_HEARTS
				     #\PLAYING_CARD_EIGHT_OF_HEARTS
				     #\PLAYING_CARD_NINE_OF_HEARTS
				     #\PLAYING_CARD_TEN_OF_HEARTS
				     #\PLAYING_CARD_JACK_OF_HEARTS
				     #\PLAYING_CARD_QUEEN_OF_HEARTS
				     #\PLAYING_CARD_KING_OF_HEARTS
				     #\PLAYING_CARD_ACE_OF_DIAMONDS
				     #\PLAYING_CARD_TWO_OF_DIAMONDS
				     #\PLAYING_CARD_THREE_OF_DIAMONDS
				     #\PLAYING_CARD_FOUR_OF_DIAMONDS
				     #\PLAYING_CARD_FIVE_OF_DIAMONDS
				     #\PLAYING_CARD_SIX_OF_DIAMONDS
				     #\PLAYING_CARD_SEVEN_OF_DIAMONDS
				     #\PLAYING_CARD_EIGHT_OF_DIAMONDS
				     #\PLAYING_CARD_NINE_OF_DIAMONDS
				     #\PLAYING_CARD_TEN_OF_DIAMONDS
				     #\PLAYING_CARD_JACK_OF_DIAMONDS
				     #\PLAYING_CARD_QUEEN_OF_DIAMONDS
				     #\PLAYING_CARD_KING_OF_DIAMONDS
				     #\PLAYING_CARD_ACE_OF_CLUBS
				     #\PLAYING_CARD_TWO_OF_CLUBS
				     #\PLAYING_CARD_THREE_OF_CLUBS
				     #\PLAYING_CARD_FOUR_OF_CLUBS
				     #\PLAYING_CARD_FIVE_OF_CLUBS
				     #\PLAYING_CARD_SIX_OF_CLUBS
				     #\PLAYING_CARD_SEVEN_OF_CLUBS
				     #\PLAYING_CARD_EIGHT_OF_CLUBS
				     #\PLAYING_CARD_NINE_OF_CLUBS
				     #\PLAYING_CARD_TEN_OF_CLUBS
				     #\PLAYING_CARD_JACK_OF_CLUBS
				     #\PLAYING_CARD_QUEEN_OF_CLUBS
				     #\PLAYING_CARD_KING_OF_CLUBS)))

(defun make-shuffled-deck (&optional (source *standard-deck*))
  "Make a newly shuffled deck of cards based on SOURCE.
With no SOURCE parameter supplied will make a standard 52 card
deck. Uses the Fisher-Yates \"inside-out\" algorithm from Wikipedia:
http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_.22inside-out.22_algorithm."
  (loop with deck = (make-array (array-dimension source 0) :element-type (array-element-type source))
	for i from 0 below (array-dimension source 0)
	for j = (random (1+ i))
	when (/= j i) do (setf (aref deck i) (aref deck j))
	do (setf (aref deck j) (aref source i))
	finally (return deck)))
