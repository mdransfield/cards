;;;; cards.lisp

(proclaim '(optimize (debug 3)))

(in-package #:cards)

(defun cardp (c)
  "Predicate true when C is a valid playing card."
  (and (characterp c)
       (<= (char-code #\PLAYING_CARD_ACE_OF_SPADES)
	   (char-code c)
	   (char-code #\PLAYING_CARD_WHITE_JOKER))))

(deftype card ()
  `(satisfies cardp))

(defclass pack ()
  ((cards :accessor cards
	  :initarg :cards)
   (top :accessor top
	:initarg :top
	:initform 0)))

(defclass standard-pack (pack)
  ((cards :accessor cards
	  :initform (make-array 52 :element-type 'card
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
						       #\PLAYING_CARD_KING_OF_CLUBS))))
  (:documentation "This is a standard Anglo-American pack of playing cards."))


(defun make-pack (cards)
  (make-instance 'pack :cards cards))

(defun make-standard-pack ()
  (make-instance 'standard-pack))

(defgeneric shuffle (pack)
  (:documentation "Shuffle PACK randomly."))

(defmethod shuffle ((pack standard-pack))
  "Shuffle PACK using the Fisher-Yates/Durstenfield algorithm from
http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle."
  (loop for i from (1- (array-dimension (cards pack) 0)) downto 0
	for j = (random (1+ i))
	do (psetf (aref (cards pack) j) (aref (cards pack) i)
		  (aref (cards pack) i) (aref (cards pack) j))))

(defgeneric cut (pack d)
  (:documentation "Cut PACK at card D."))

(defmethod cut ((pack standard-pack) d)
  "Rotate PACK's cards at index D."
  (left-rotate (cards pack) d (array-dimension (cards pack) 0)))

(defun left-rotate (arr d n)
  "Rotate ARR of length N starting from index S around index D.
The algorithm is Gries--Mills block-swap (described in
http://www.cs.bell-labs.com/cm/cs/pearls/s02b.pdf).

The algorithm is this:

1. Initialize A := ARR[0..D-1] and B := ARR[D..N-1].

2. Repeat until length(A) == length(B)

   a. If length(A) < length(B), divide B into B_l and B_r such that
      length(B_r) == length(A).  Swap A with B_r (ARR goes from
      AB_lB_r to B_rB_lA).  Now A is at its eventual position, recur
      on the pieces of B.

   b. If length(A) > length(B), divide A into A_l and A_r such that
      length(A_l) == length(B).  Swap A_l with B (ARR goes from
      A_lA_rB to BA_rA_l).  Now B is at its eventual position, recur
      on the pieces of A.

3. Swap A and B."
;  (format t "~&(left-rotate ~a ~a ~a ~a)~%" arr s d n)
  (unless (or (= d 0) (= d n))
    (loop with i = d
	  with j = (- n d)
	  while (/= i j)
	  if (< i j)
	    do (swap arr (- d i) (- (+ d j) i) i)
	       (setf j (- j i))
	  else
	    do (swap arr (- d i) d j)
	       (setf i (- i j))
	  finally (swap arr (- d i) d i))))

(defun swap (arr fi si d)
  "Rotate D elements of ARR starting at FI with D elements starting at SI."
;  (format t "~&(swap ~a ~a ~a ~a)~%" arr d fi si)
  (loop for i from 0 below d
	for f = (+ fi i)
	for s = (+ si i)
	do (psetf (aref arr f) (aref arr s)
		  (aref arr s) (aref arr f))))
