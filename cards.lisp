;;;; cards.lisp

(in-package #:cards)

(defun cardp (c)
  "Predicate true when C is a valid playing card."
  (and (characterp c)
       (<= (char-code #\PLAYING_CARD_ACE_OF_SPADES)
	   (char-code c)
	   (char-code #\PLAYING_CARD_WHITE_JOKER))))

(deftype card ()
  `(satisfies cardp))

(defclass deck ()
  ((cards :accessor cards
	  :initarg :cards
	  :initform (make-shuffled-cards))
   (top :accessor top
	:initarg :top
	:initform 0)))

(defconstant standard-cards
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

(defun make-shuffled-cards (&optional (source standard-cards))
  "Make a newly shuffled array of cards based on SOURCE.
With no SOURCE parameter supplied will make a standard 52 card
deck. Uses the Fisher-Yates \"inside-out\" algorithm from Wikipedia:
http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_.22inside-out.22_algorithm."
  (loop with deck = (make-array (array-dimension source 0) :element-type (array-element-type source))
	for i from 0 below (array-dimension source 0)
	for j = (random (1+ i))
	when (/= j i) do (setf (aref deck i) (aref deck j))
	do (setf (aref deck j) (aref source i))
	finally (return deck)))
