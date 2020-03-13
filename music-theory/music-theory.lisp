;;;; music-theory.lisp
;;;;
;;;; Author: Robert Smith

(cl:defpackage #:music-theory
  (:use #:coalton)
  (:import-from #:coalton-user
                #:identity
                #:compose
                #:+ #:- #:* #:/ #:1+ #:1-
                #:= #:> #:>= #:< #:<=
                #:minusp #:plusp
                #:evenp #:oddp
                #:error))

;;; This file primarily deals with reconstructing the circle of fifths
;;; from first principles. We take an "algebraic" approach, in the
;;; sense that we do our constructions as formal algebraic
;;; manipulations. With this approach, we take many definitions for
;;; granted, which will be noted below.

(in-package #:music-theory)

;;; Utilities
(coalton-toplevel
  (declare mod (-> (Integer Integer) Integer))
  (define (mod n k)
    (lisp Integer
      (cl:mod n k)))

  (define (fexpt f n)
    (cond
      ((minusp n) (error "domain"))
      (else
       (cond
         ((= n 0) identity)
         ((= n 1) f)
         ((evenp n)
          (let ((g (fexpt f (/ n 2))))
            (compose g g)))
         ((oddp n)
          (compose f (fexpt f (1- n))))
         (else
          (error "unreachable"))))))
  
  (declare strcat (-> (String String) String))
  (define (strcat a b)
    (lisp String
      (cl:concatenate 'cl:string a b))))


;;; Naming notes
(coalton-toplevel
  
  ;; First we define what a note is. This is specifically the name of
  ;; a chromatic note.
  ;;
  ;; We append them with a period just to make them a little more
  ;; obvious, and to not pollute the namespace too much.
  ;;
  ;; One possible extension of this is to actually include from which
  ;; octave of the keyboard the notes come from.
  
  (define-type note
    A. B. C. D. E. F. G.
    (sharp note)
    (flat note))
  
  (define (note-string n)
    (match n
      (A. "A")
      (B. "B")
      (C. "C")
      (D. "D")
      (E. "E")
      (F. "F")
      (G. "G")
      ((sharp n) (strcat (note-string n) "#"))
      ((flat n)  (strcat (note-string n) "b"))))

  (define (chromatic-degree->note k)
    (cond                               ; This function looks like a
                                        ; piano!
      ((= k 0)  C.)
      ((= k 1)  (sharp C.))
      ((= k 2)  D.)
      ((= k 3)  (sharp D.))
      ((= k 4)  E.)
      ((= k 5)  F.)
      ((= k 6)  (sharp F.))
      ((= k 7)  G.)
      ((= k 8)  (sharp G.))
      ((= k 9)  A.)
      ((= k 10) (sharp A.))
      ((= k 11) B.)
      (else (chromatic-degree->note (mod k 12)))))
  
  (define (note->chromatic-degree n)
    (match n
      (C. 0)
      (D. 2)
      (E. 4)
      (F. 5)
      (G. 7)
      (A. 9)
      (B. 11)
      ((sharp n) (1+ (note->chromatic-degree n)))
      ((flat n)  (1- (note->chromatic-degree n))))))

;;; Semitones, tones, and canonical notes
(coalton-toplevel
  (define (semitone-up n k)
    (chromatic-degree->note (+ k (note->chromatic-degree n))))
  
  (define (tone-up n k)
    (semitone-up n (* 2 k)))
  
  (define (canonical n)
    (chromatic-degree->note (note->chromatic-degree n))))
