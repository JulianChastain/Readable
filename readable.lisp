;;; basic-curly.cl
;;; This implements "basic curly-infix-expressions" for Common Lisp.
;;; This is an easy-to-use infix notation that works
;;; well with other Common Lisp capabilities (e.g., quasiquoting and macros).
;;; Basically, {a op b op c ...} => (op a b c ....).
;;; It's homoiconic (you can see where lists start and end) and easy to use
;;; (e.g., no need to register operators).  For more information, see:
;;;   http://readable.sourceforge.net.
;;;
;;; Copyright (C) 2007-2013 by David A. Wheeler
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.


; This implements an basic-curly-infix reader, an extension of s-expressions
; that can read "basic curly-infix lists".  A basic curly-infix list
; is surrounded by {...} instead (...), and the reader maps it as follows:
; * {a op b [op c [op d ...]]} => (op a b [c [d ...]]).
;   E.G., {x > 1} => (> x 1), and {a + b + c} => (+ a b c)
;   In short, a curly-infix list with an odd number of parameters, at least
;   three parameters, and all even parameters are "equal", maps to
;   a list with the first even parameter followed by the odd parameters.
; * {} => ()
; * {e} => e, so {5} => 5.
; * {e1 e2} => (e1 e2), e.g., {- x} => (- x).
; * Otherwise, {...} maps to ($nfx$ ...).
; A non-empty basic curly-infix list must be a proper list.
;
; There's no precedence, but that's no problem,
; just use another curly-infix list or traditional list:
;   {2 + {4 * 5}} => (+ 2 (* 4 5))
;   {{- x} / 2}   => {(- x) / 2}   => (/ (- x) 2)

(cl:in-package :readable)

(defvar *original-readtable* (copy-readtable) "Saved readtable")
(defvar *readable-active* nil "Value of active readable notation for reading")
(defvar *print-notation* nil "Value of readable notation for printing")

; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns nil (false).
; If passed empty list, returns true (so recursion works correctly).
(defun even-and-op-prefixp (op lyst)
   (cond
     ((null lyst) t)
     ((not (consp lyst)) nil) ; Not a list.
     ((not (equal op (car lyst))) nil) ; fail - operators not all equal.
     ((not (consp (cdr lyst))) nil) ; fail - wrong # or improper list.
     (t (even-and-op-prefixp op (cddr lyst))))) ; recurse.

; Return true if the lyst is in simple infix format (and should be converted
; at read time).  Else returns nil.
(defun simple-infix-listp (lyst)
  (and
    (consp lyst)           ; Must have cons;  '() doesn't count.
    (consp (cdr lyst))     ; Must have a second argument.
    (consp (cddr lyst))    ; Must have a third argument (we check it
                           ; this way for performance)
    (even-and-op-prefixp (cadr lyst) (cdr lyst)))) ; even parameters equal?

; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(defun alternating-parameters (lyst)
  (if (or (null lyst) (null (cdr lyst)))
    lyst
    (cons (car lyst) (alternating-parameters (cddr lyst)))))

; Transform not-simple infix list.  Written as a separate function so that
; future versions/specifications can easily replace just this piece.
(defun transform-mixed-infix (lyst)
  (cons '$nfx$ lyst))

; Take list that was in {...}, convert to final form.
(defun process-curly (lyst)
  (cond
    ((not (consp lyst)) ; E.G., map {} to ().
      lyst)
    ((null (cdr lyst)) ; Map {a} to a.
      (car lyst))
    ((and (consp (cdr lyst)) (null (cddr lyst))) ; Map {a b} to (a b).
      lyst)
    ((simple-infix-listp lyst) ; Map {a op b} to (op a b).
      (cons (cadr lyst) (alternating-parameters lyst)))
    (t
      (transform-mixed-infix lyst))))

; Read until }, then process list as infix list.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun curly-brace-infix-reader (stream char)
    (declare (ignore char))
    (let ((result (read-delimited-list #\} stream t)))
      (process-curly result))))

; Enable setup to transition into "into":
; If we're already in mode "into" return false (nil) and do nothing.
; Otherwise, get it ready to transition to the new mode and return true.
; If we transition, set *print-notation* to the new notation.
(defun setup-enable (into)
  (cond
    ((eq *readable-active* into) nil) ; Do nothing.
    ((not *readable-active*) ; In no mode at all, start with THIS readtable
      (setq *original-readtable* *readtable*)
      (setq *readtable* (copy-readtable))
      (setq *readable-active* t)
      (setq *print-notation* into)
      t)
    (t ; We are changing from one readable mode to another; recover readtable
      (setq *readtable* (copy-readtable *original-readtable*))
      (setq *print-notation* into)
      t)))

(defun enable-basic-curly-real ()
  (in-readtable readable:basic-curly-infix))

					;
; Should be able to do this without eval-when, look at cmu-infix
(eval-when (:compile-toplevel :load-toplevel)
    (defreadtable readable:basic-curly-infix
	(:merge :standard)
	; The following install the {...} reader.
	; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
	; pp. 542-548 and pp. 571-572.
	; Invoke curly-brace-infix-reader when opening curly brace is read in:
	(:macro-char #\{ #'curly-brace-infix-reader)
	; Necessary, else a cuddled closing brace will be part of an atom.
	(:macro-char #\} (get-macro-character #\) nil))))

(defun disable-readable ()
  (when *readable-active*
    (setq *readtable* *original-readtable*)
    (setq *readable-active* nil))
  (values))


(defun basic-curly-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-basic-curly-real)
    (read stream)))

;; Print.cl
;;;; Output routines for readable notations.
;;;; Use *print-notation* to decide what notation to use when writing,
;;;; which may be 'basic-curly-infix, 'full-curly-infix, 'neoteric, or 'sweet.

;;;; Some of this code is derived from SBCL, which was in turn
;;;; derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain.
;;;; Thus, this is derived from public domain code.
;;;; Much of the rest of the code is derived from the Scheme implementation
;;;; of the readable notation, which is also licensed under the MIT license.

;;;; Copyright (C) 2007-2014 by David A. Wheeler
;;;; This software is released as open source software under the "MIT" license:
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; The key function here is output-object-readable; outer functions call it,
;;;; and internal printing functions keep recursing back to it.



(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(defvar *suppress-print-errors* nil
  ;; Suppress printer errors when the condition is of the type designated by this variable: 
  ;; an unreadable object representing the error is printed instead.
  )


(defvar *print-notation* nil
  ;; Currently-active notation used in readable package's writers
  )

; Track errors in output-object-readable:
(defvar *readable-in-print-error* nil)

; TODO: Determine output stream.  Just return stream provided for now.
(defun out-synonym-of (stream)
  stream)

(defun allow-neoteric ()
  (member *print-notation* '(neoteric sweet)))

; The following functions are derived from the Scheme implementation
; in "kernel.scm"

; A list with more than this length and no pairs is considered "boring",
; and thus is presumed to NOT be a procedure call or execution sequence.
(define-constant boring-length 16)

(define-constant special-infix-operators '(and or xor))

(define-constant punct-chars
  `(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
    #\.  #\/ #\: #\; #\< #\= #\> #\p #\@ #\[ #\\ #\] #\^
    #\- #\` #\{ #\| #\} #\~ ))

; Returns t if x is a list with exactly 1 element.  Improper lists are nil.
(defun list1p (x)
  (and (consp x) (null (cdr x))))

; Returns t if x is a list with exactly 2 elements.  Improper lists are nil.
(defun list2p (x)
  (and (consp x) (consp (cdr x)) (null (cddr x))))

; Does x contain a list of ONLY punctuation characters?
; An empty list is considered true.
(defun contains-only-punctuationp (x)
  (cond ((null x) t)
        ((atom x) nil)
        ((member (car x) punct-chars)
         (contains-only-punctuationp (cdr x)))
        (t nil)))

; Returns t if x is a symbol that would typically be used in infix position.
(defun is-infix-operatorp (x)
  (cond ((not (symbolp x)) nil)
        ((member x special-infix-operators) t)
        (t
         (contains-only-punctuationp
           (coerce (symbol-name x) 'list)))))

; A possibly-improper list is long and boring if its length is at least
; num-to-go long and it's boring (it contains no pairs up to that length).
; A long-and-boring list is almost certainly NOT a function call or a
; body of some executable sequence - it's almost certainly a long
; boring list of data instead. If it is, we want to display it differently.
; This doesn't get stuck on circular lists; it always terminates after
; num-to-go iterations.
(defun long-and-boringp (x num-to-go)
  (cond
    ((consp (car x)) nil)
    ((not (consp (cdr x))) nil)
    ((<= num-to-go 1) t)
    (t (long-and-boringp (cdr x) (- num-to-go 1)))))

(defun list-no-longer-thanp (x num-to-go)
  (cond
    ((atom x) nil)
    ((null (cdr x)) t) ; This is the last one!
    ((not (consp (cdr x))) nil)
    ((<= num-to-go 0) nil)
    (t (list-no-longer-thanp (cdr x) (- num-to-go 1)))))

; Return t if x should be represented using curly-infix notation {...}.
(defun represent-as-infixp (x)
  (and (consp x)
       (consp (cdr x))                ; At least 2 elements.
       (is-infix-operatorp (car x))
       (list-no-longer-thanp x 6)))

(defun represent-as-inline-infixp (x)
  (and (represent-as-infixp x) (not (list2p x)))) ; Must be 3+ elements

; Return t if x should be represented as a brace suffix
(defun represent-as-brace-suffixp (x)
  (represent-as-infixp x))

; Define an association list mapping the Scheme procedure names which have
; abbreviations ==> the list of characters in their abbreviation
(define-constant abbreviations
  '((quote (#\'))
    (function (#\# #\'))))

; return t if we should as a traditional abbreviation, e.g., '
(defun represent-as-abbreviationp (x)
  (and (list2p x)
       (assoc (car x) abbreviations)))

; The car(x) is the symbol for an abbreviation; write the abbreviation.
(defun write-abbreviation (x stream)
  (mapc (lambda (c) (princ c stream))
    (cadr (assoc (car x) abbreviations))))

; Write rest of list x's contents.
(defun write-list-contents-rest (x stream)
  (cond
    ((null x) (values))
    ((check-for-circularity x)
      (write-string " . " stream)
      (output-object-readable x stream))
    ((consp x)
      (princ " " stream)
      (output-object-readable (car x) stream)
      (write-list-contents-rest (cdr x) stream))
    (t
      (princ " . " stream)
      (output-object-readable x stream))))

(defun write-list-contents (x stream)
  (cond
    ((null x) (values))
    ((consp x)
      (output-object-readable (car x) stream)
      (write-list-contents-rest (cdr x) stream))
    (t
      (princ ". " stream)
      (output-object-readable x stream))))

; Return tail of an infix expression, as list of chars
; The "op" is the infix operator represented as a list of chars.
(defun infix-tail (op x stream)
  (cond
    ((null x) (princ "}" stream))
    ((consp x)
      (princ " " stream)
      (output-object-readable op stream)
      (princ " " stream)
      (output-object-readable (car x) stream)
      (infix-tail op (cdr x) stream))
    (t
      (princ " " stream)
      (output-object-readable x stream)
      (princ "}" stream))))

; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
(defun as-brace-suffix (x stream)
  (princ "{" stream)
  (if (list2p x)
    (progn
      (write-list-contents x stream)
      (princ "}" stream))
    (progn
      (output-object-readable (cadr x) stream)
      (infix-tail (car x) (cddr x) stream))))

;;; Main routine for outputting objects in current readable notation.
;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
(defun output-ugly-object-readable (object stream)
  (typecase object
    (list
      (cond
        ((null object)
         (write object :stream stream))
        ((represent-as-abbreviationp object)              ; Format 'x
          (write-abbreviation object stream)
          (output-object-readable (cadr object) stream))
        ((represent-as-inline-infixp object)              ; Format {a + b}
          (princ "{" stream)
          (let ((*print-notation*
                  (if (eq *print-notation* 'full-curly-infix)
                      'neoteric
                      *print-notation*)))
               (output-object-readable (cadr object) stream)
               (infix-tail (car object) (cddr object) stream)))
        ((and (allow-neoteric)
              (long-and-boringp object boring-length))    ; Format (a b c ...)
          (princ "(" stream)
          (write-list-contents object stream)
          (princ ")" stream))
        ((and (allow-neoteric)                            ; Format f{...}
              (symbolp (car object))
              (list1p (cdr object))
              (consp (cadr object))
              (represent-as-infixp (cadr object)))
          (output-object-readable (car object) stream)
          (as-brace-suffix (cadr object) stream))
        ((and (allow-neoteric)                            ; Format f(...)
              (symbolp (car object)))
          (output-object-readable (car object) stream)
          (princ "(" stream)
          (write-list-contents (cdr object) stream)
          (princ ")" stream))
        (t                                                ; Format (1 2 3 ...)
          (princ "(" stream)
          (write-list-contents object stream)
          (princ ")" stream))))
    (symbol
      ; Workaround: clisp displays symbols oddly if readtable set
      #+clisp
      (let ((*readtable* *original-readtable*))
        (write object :stream stream))
      #-clisp
      (write object :stream stream))
    ; TODO: Many other types, including vectors, structures, hash tables, etc.
    ; ((vectorp x)
    ;   (princ "#( " stream) ; Surround with spaces, easier to implement.
    ;   (mapc (lambda (v) (c-write-simple v stream) (princ " " stream))
    ;     (vector->list x))
    ;   (princ ")" stream))
    (t
      (write object :stream stream))))

; TODO: Implement fully.
;;; Could this object contain other objects? (This is important to
;;; the implementation of things like *PRINT-CIRCLE* and the dumper.)
(defun compound-object-p (x)
  (or (consp x)
      ; (%instancep x)
      (typep x '(array t *))))
(defun setup-printer-state ()
  nil)


;;;; circularity detection stuff

;;; When *PRINT-CIRCLE* is T, this gets bound to a hash table that
;;; (eventually) ends up with entries for every object printed. When
;;; we are initially looking for circularities, we enter a T when we
;;; find an object for the first time, and a 0 when we encounter an
;;; object a second time around. When we are actually printing, the 0
;;; entries get changed to the actual marker value when they are first
;;; printed.
(defvar *circularity-hash-table* nil)

;;; When NIL, we are just looking for circularities. After we have
;;; found them all, this gets bound to 0. Then whenever we need a new
;;; marker, it is incremented.
(defvar *circularity-counter* nil)

;;; Check to see whether OBJECT is a circular reference, and return
;;; something non-NIL if it is. If ASSIGN is true, reference
;;; bookkeeping will only be done for existing entries, no new
;;; references will be recorded. If ASSIGN is true, then the number to
;;; use in the #n= and #n# noise is assigned at this time.
;;;
;;; Note: CHECK-FOR-CIRCULARITY must be called *exactly* once with
;;; ASSIGN true, or the circularity detection noise will get confused
;;; about when to use #n= and when to use #n#. If this returns non-NIL
;;; when ASSIGN is true, then you must call HANDLE-CIRCULARITY on it.
;;; If CHECK-FOR-CIRCULARITY returns :INITIATE as the second value,
;;; you need to initiate the circularity detection noise, e.g. bind
;;; *CIRCULARITY-HASH-TABLE* and *CIRCULARITY-COUNTER* to suitable values
;;; (see #'OUTPUT-OBJECT for an example).
;;;
;;; Circularity detection is done in two places, OUTPUT-OBJECT and
;;; WITH-CIRCULARITY-DETECTION (which is used from PPRINT-LOGICAL-BLOCK).
;;; These checks aren't really redundant (at least I can't really see
;;; a clean way of getting by with the checks in only one of the places).
;;; This causes problems when mixed with pprint-dispatching; an object is
;;; marked as visited in OUTPUT-OBJECT, dispatched to a pretty printer
;;; that uses PPRINT-LOGICAL-BLOCK (directly or indirectly), leading to
;;; output like #1=#1#. The MODE parameter is used for detecting and
;;; correcting this problem.
(defun check-for-circularity (object &optional assign (mode t))
  (cond ((null *print-circle*)
         ;; Don't bother, nobody cares.
         nil)
        ((null *circularity-hash-table*)
          (values nil :initiate))
        ((null *circularity-counter*)
         (ecase (gethash object *circularity-hash-table*)
           ((nil)
            ;; first encounter
            (setf (gethash object *circularity-hash-table*) mode)
            ;; We need to keep looking.
            nil)
           ((:logical-block)
            (setf (gethash object *circularity-hash-table*)
                  :logical-block-circular)
            t)
           ((t)
            (cond ((eq mode :logical-block)
                   ;; We've seen the object before in output-object, and now
                   ;; a second time in a PPRINT-LOGICAL-BLOCK (for example
                   ;; via pprint-dispatch). Don't mark it as circular yet.
                   (setf (gethash object *circularity-hash-table*)
                         :logical-block)
                   nil)
                  (t
                   ;; second encounter
                   (setf (gethash object *circularity-hash-table*) 0)
                   ;; It's a circular reference.
                   t)))
           ((0 :logical-block-circular)
            ;; It's a circular reference.
            t)))
        (t
         (let ((value (gethash object *circularity-hash-table*)))
           (case value
             ((nil t :logical-block)
              ;; If NIL, we found an object that wasn't there the
              ;; first time around. If T or :LOGICAL-BLOCK, this
              ;; object appears exactly once. Either way, just print
              ;; the thing without any special processing. Note: you
              ;; might argue that finding a new object means that
              ;; something is broken, but this can happen. If someone
              ;; uses the ~@<...~:> format directive, it conses a new
              ;; list each time though format (i.e. the &REST list),
              ;; so we will have different cdrs.
              nil)
             ;; A circular reference to something that will be printed
             ;; as a logical block. Wait until we're called from
             ;; PPRINT-LOGICAL-BLOCK with ASSIGN true before assigning the
             ;; number.
             ;;
             ;; If mode is :LOGICAL-BLOCK and assign is false, return true
             ;; to indicate that this object is circular, but don't assign
             ;; it a number yet. This is neccessary for cases like
             ;; #1=(#2=(#2# . #3=(#1# . #3#))))).
             (:logical-block-circular
              (cond ((and (not assign)
                          (eq mode :logical-block))
                     t)
                    ((and assign
                          (eq mode :logical-block))
                     (let ((value (incf *circularity-counter*)))
                       ;; first occurrence of this object: Set the counter.
                       (setf (gethash object *circularity-hash-table*) value)
                       value))
                    (t
                     nil)))
             (0
              (if (eq assign t)
                  (let ((value (incf *circularity-counter*)))
                    ;; first occurrence of this object: Set the counter.
                    (setf (gethash object *circularity-hash-table*) value)
                    value)
                  t))
             (t
              ;; second or later occurrence
              (- value)))))))

;;; Handle the results of CHECK-FOR-CIRCULARITY. If this returns T then
;;; you should go ahead and print the object. If it returns NIL, then
;;; you should blow it off.
(defun handle-circularity (marker stream)
  (case marker
    (:initiate
     ;; Someone forgot to initiate circularity detection.
     (let ((*print-circle* nil))
       (error "trying to use CHECK-FOR-CIRCULARITY when ~
               circularity checking isn't initiated")))
    ((t :logical-block)
     ;; It's a second (or later) reference to the object while we are
     ;; just looking. So don't bother groveling it again.
     nil)
    (t
     (write-char #\# stream)
     (let ((*print-base* 10) (*print-radix* nil))
       (cond ((minusp marker)
              (write (- marker) :stream stream)
              (write-char #\# stream)
              nil)
             (t
              (write marker :stream stream)
              (write-char #\= stream)
              t))))))


;;; Objects whose print representation identifies them EQLly don't
;;; need to be checked for circularity.
(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (and (symbolp x)
           (symbol-package x))))

;;; Output OBJECT to STREAM observing all printer control variables.
;;; This code is straight from SBCL, including its hairiness.
(defun output-object-readable (object stream)
  (labels ((print-it (stream)
             (if *print-pretty*
                 ; TODO: (sb!pretty:output-pretty-object object stream)
                 (output-ugly-object-readable object stream)
                 (output-ugly-object-readable object stream)))
           (handle-it (stream)
             (if *suppress-print-errors*
                 (handler-bind ((condition
                                  (lambda (condition) nil
                                    (when (typep condition *suppress-print-errors*)
                                      (cond (*readable-in-print-error*
                                             (write-string "(error printing " stream)
                                             (write-string *readable-in-print-error* stream)
                                             (write-string ")" stream))
                                            (t
                                             ;; Give outer handlers a chance.
                                             (with-simple-restart
                                                 (continue "Suppress the error.")
                                               (signal condition))
                                             (let ((*print-readably* nil)
                                                   (*print-escape* t))
                                               (write-string
                                                "#<error printing a " stream)
                                               (let ((*readable-in-print-error* "type"))
                                                 (output-object-readable (type-of object) stream))
                                               (write-string ": " stream)
                                               (let ((*readable-in-print-error* "condition"))
                                                 (output-object-readable condition stream))
                                               (write-string ">" stream))))
                                      (return-from handle-it object)))))
                   (print-it stream))
                 (print-it stream)))
           (check-it (stream)
             (multiple-value-bind (marker initiate)
                 (check-for-circularity object t)
               (if (eq initiate :initiate)
                   (let ((*circularity-hash-table*
                          (make-hash-table :test 'eq)))
                     (check-it (make-broadcast-stream))
                     (let ((*circularity-counter* 0))
                       (check-it stream)))
                   ;; otherwise
                   (if marker
                       (when (handle-circularity marker stream)
                         (handle-it stream))
                       (handle-it stream))))))
    (cond (;; Maybe we don't need to bother with circularity detection.
           (or (not *print-circle*)
               (uniquely-identified-by-print-p object))
           (handle-it stream))
          (;; If we have already started circularity detection, this
           ;; object might be a shared reference. If we have not, then
           ;; if it is a compound object it might contain a circular
           ;; reference to itself or multiple shared references.
           (or *circularity-hash-table*
               (compound-object-p object))
           (check-it stream))
          (t
           (handle-it stream)))))

(defun write-readable (object &key
                     ((:stream stream) *standard-output*)
                     ((:escape *print-escape*) *print-escape*)
                     ((:radix *print-radix*) *print-radix*)
                     ((:base *print-base*) *print-base*)
                     ((:circle *print-circle*) *print-circle*)
                     ((:pretty *print-pretty*) *print-pretty*)
                     ((:level *print-level*) *print-level*)
                     ((:length *print-length*) *print-length*)
                     ((:case *print-case*) *print-case*)
                     ((:array *print-array*) *print-array*)
                     ((:gensym *print-gensym*) *print-gensym*)
                     ((:readably *print-readably*) *print-readably*)
                     ((:right-margin *print-right-margin*)
                      *print-right-margin*)
                     ((:miser-width *print-miser-width*)
                      *print-miser-width*)
                     ((:lines *print-lines*) *print-lines*)
                     ((:pprint-dispatch *print-pprint-dispatch*)
                      *print-pprint-dispatch*)
                     ((:suppress-errors *suppress-print-errors*)
                      *suppress-print-errors*)
                     ((:notation *print-notation*) *print-notation*))
  "Output OBJECT to the specified stream, defaulting to *STANDARD-OUTPUT*."
  (output-object-readable object (out-synonym-of stream))
  object)

(defun prin1-readable (object &optional stream)
  "Output a mostly READable printed representation of OBJECT on the specified
  STREAM."
  (let ((*print-escape* t))
    (output-object-readable object (out-synonym-of stream)))
  object)

(defun princ-readable (object &optional stream)
  "Output an aesthetic but not necessarily READable printed representation
  of OBJECT on the specified STREAM."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (output-object-readable object (out-synonym-of stream)))
  object)

(defun print-readable (object &optional stream)
  "Output a newline, the mostly READable printed representation of OBJECT, and
  space to the specified STREAM."
  (let ((stream (out-synonym-of stream)))
    (terpri stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))

(defun pprint-readable (object &optional stream)
  "Prettily output OBJECT preceded by a newline."
  (let ((*print-pretty* t)
        (*print-escape* t)
        (stream (out-synonym-of stream)))
    (terpri stream)
    (output-object-readable object stream))
  (values))


;;; This produces the printed representation of an object as a string.
;;; The few ...-TO-STRING functions call this.
(defun stringify-object-readable (object)
  (let ((stream (make-string-output-stream)))
    (setup-printer-state)
    (output-object-readable object stream)
    (get-output-stream-string stream)))

(defun write-to-string-readable
    (object &key
            ((:escape *print-escape*) *print-escape*)
            ((:radix *print-radix*) *print-radix*)
            ((:base *print-base*) *print-base*)
            ((:circle *print-circle*) *print-circle*)
            ((:pretty *print-pretty*) *print-pretty*)
            ((:level *print-level*) *print-level*)
            ((:length *print-length*) *print-length*)
            ((:case *print-case*) *print-case*)
            ((:array *print-array*) *print-array*)
            ((:gensym *print-gensym*) *print-gensym*)
            ((:readably *print-readably*) *print-readably*)
            ((:right-margin *print-right-margin*) *print-right-margin*)
            ((:miser-width *print-miser-width*) *print-miser-width*)
            ((:lines *print-lines*) *print-lines*)
            ((:pprint-dispatch *print-pprint-dispatch*)
             *print-pprint-dispatch*)
            ((:suppress-errors *suppress-print-errors*)
             *suppress-print-errors*)
            ((:notation *print-notation*) *print-notation*))
  "Return the printed representation of OBJECT as a string."
  (stringify-object-readable object))

(defun prin1-to-string-readable (object)
  "Return the printed representation of OBJECT as a string with
   slashification on."
  (let ((*print-escape* t))
    (stringify-object-readable object)))

(defun princ-to-string-readable (object)
  "Return the printed representation of OBJECT as a string with
  slashification off."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (stringify-object-readable object)))



;;; neoteric.cl
;;; Implements neoteric-expressions from the "readable" approach for Lisp.

;;; Copyright (C) 2007-2013 by David A. Wheeler
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; Neoteric-expressions themselves are a very simple notation and their
;;; basic implementation is also simple.
;;; Unfortunately, some quirks in Common Lisp (CL) mean that we have to do
;;; some work-arounds as compared to other Lisps like Scheme:
;;; 1. CL "read" by default consumes trailing whitespace, even if the
;;;    the whitespace is NOT part of the datum being read at all.
;;;    This is an unfortunate quirk, and in my view, a bug.  As a result,
;;;    if you just do a "read", a later datum may look
;;;    like a neoteric tail.  E.G., given: "x (y)", a simple read of "x"
;;;    will consume the space after "x"; naively checking for a "tail"
;;;    would them make it look like "x(y)" instead, wrongly producing "(x y").
;;;    For example, if we naively pass down "recursive" as "t" in
;;;    all places, it'll use this default and consume trailing whitespace.
;;;    Thus, we have to be careful about calling read or calling any
;;;    reads with recursive=t. Instead, we'll typically call
;;;    "read-preserving-whitespace", call with recursive=nil, or specially
;;;    extract characters into a string for reading.
;;; 2. There's no portable way to directly replace the "read" procedure,
;;;    so we must manipulate the readtable to do what we want.
;;;    We end up wrapping all constituents so that we can prevent consuming
;;;    trailing whitespace, to distinguish "x(y)" from "x (y)".

(defvar *neoteric-underlying-readtable* (copy-readtable)
        "Use this table when reading neoteric atoms")

(defvar neoteric-readtable* (copy-readtable)
        "Use this table when about to read a neoteric expression")


(defvar *noisy* t
        "If true, a parse error prints an error message to standard out")

; Work around SBCL nonsense that makes its "defconstant" useless.
; See: http://www.sbcl.org/manual/Defining-Constants.html
; This is disabled, because we already define it in "print".
; (defmacro define-constant (name value &optional doc)
;  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
;                      ,@(when doc (list doc))))

; Marker for eof
(define-constant my-eof-marker (make-symbol "my-eof-marker"))

(define-condition readable-parse-error (parse-error)
  ((text :initarg :text :reader text)))

(defun read-error (message)
  (when *noisy*
    (terpri *standard-output*)
    (princ "Error: " *standard-output*)
    (princ message *standard-output*)
    (terpri *standard-output*))
  (error 'readable-parse-error :text message))

; Unfortunately, clisp will write all symbols with |...| around them
; when neoteric- or sweet-expressions are enabled.
; The issue is in clisp file "src/io.d" function "pr_symbol_part".
; We hope to be able to modify clisp in the future so it can be disabled,
; but for now, just carry one.

; NOTE: clisp's "peek-char" has a serious bug; it defaults to CONSUME
; a following whitespace, contravening the Common Lisp spec:
;    http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm
; We work around this by ALWAYS providing peek-char with 2 parameters
; ("nil" and the stream name) when we don't want to skip whitespace.

; Test to ensure that peek-char (as we use it) works.
(with-input-from-string (test-input "Q56 T78")
  (progn
    ; Read "Q56"; this should NOT consume the space after it.
    (read-preserving-whitespace test-input t nil)
    (let ((c (peek-char nil test-input)))
      (when (not (eql c #\space))
        (terpri) (terpri) (terpri)
        (princ "*** WARNING WARNING WARNING ***") (terpri)
        (princ "Procedure read-preserving-whitespace or peek-char") (terpri)
        (princ "FAIL to preserve whitespace following expressions.") (terpri)
        (princ "*** WARNING WARNING WARNING ***") (terpri)
        (terpri) (terpri) (terpri)
        (error "peek-char BUG")))))

; Set of constituents - these will be overridden.
; We should support arbitrary UTF-8 characters, but it can be complicated
; to do so portably.  For now, we'll define this as a variable so it
; can be overridden.
(eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *constituents*
	'(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
	#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
	#\: #\< #\= #\> #\? #\@
	#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
	#\^ #\_
	#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~
	#\rubout ))) ; Rubout, really?!?  Yup, it's in the spec.

; TODO: Add UTF-8 constituents to *constituents* if the underlying
; system supports them in the readtable.

;;; Key procedures to implement neoteric-expressions

; These delimiting characters stop reading of symbols or non-datums
; (e.g., after ".").  We'll define these as a "variable" so that other
; routines can reach in and change them, if truly necessary.
(defvar neoteric-delimiters
  '(#\( #\) #\[ #\] #\{ #\} #\space #\tab #\newline #\return #\#
    #\' #\` #\,))

(defun string-before-delimiter (input-stream start)
  (concatenate 'string start
    (loop for c = (peek-char nil input-stream nil my-eof-marker)
          until (or (eq c my-eof-marker) (find c neoteric-delimiters))
          collect (read-char input-stream))))

; Note: In traditional Common Lisp, symbols with just multiple "."
; are illegal, especially "...".  For the moment we'll allow it.

; Read a datum and ALLOW "." as a possible value:
(defun my-read-to-delimiter (input-stream start)
  (let* ((*readtable* *neoteric-underlying-readtable*) ; Temporary switch
         (my-string (string-before-delimiter input-stream start)))
    (if (string= my-string ".")
        '|.|
        (read-from-string my-string))))

(defun my-read-datum (input-stream)
  (let* ((c (peek-char t input-stream))) ; Consume leading whitespace
    (cond
      ((eql c #\.) ; Use specialized reader if starts with "."
        (my-read-to-delimiter input-stream ""))
      (t (read-preserving-whitespace input-stream t nil)))))

(defun my-read-delimited-list (stop-char input-stream)
 (handler-case
  (let* ((c (peek-char t input-stream))) ; First consume leading whitespace
    (cond
      ((eql c stop-char)
        (read-char input-stream)
        '())
      ; Balance ([{
      ((or (eql c #\)) (eql c #\]) (eql c #\}))
        (read-char input-stream)
        (read-error "Bad closing character."))
      (t
        ; Must preserve whitespace so "a ()" isn't read as "a()"
        (let ((datum (my-read-datum input-stream)))
          (cond
             ; Note: "." only counts as cdr-setting if it begins with "."
             ((and (eq datum '|.|) (eql c #\.))
               (let ((datum2 (read-preserving-whitespace input-stream t nil)))
                 ; (consume-whitespace input-stream)
                 (cond
                   ; ((eof-object? datum2)
                   ; (read-error "Early eof in (... .).")
                   ; '())
                   ; The following peek-char has side-effect of skipping
                   ; whitespace after last datum, so "(a . b )" works.
                   ((not (eql (peek-char t input-stream) stop-char))
                    (read-error "Bad closing character after . datum."))
                   (t
                     (read-char input-stream)
                     datum2))))
             (t
                 (cons datum
                   (my-read-delimited-list stop-char input-stream))))))))))


; Implement neoteric-expression's prefixed (), [], and {}.
; At this point, we have just finished reading some expression, which
; MIGHT be a prefix of some longer expression.  Examine the next
; character to be consumed; if it's an opening paren, bracket, or brace,
; then the expression "prefix" is actually a prefix.
; Otherwise, just return the prefix and do not consume that next char.
; This recurses, to handle formats like f(x)(y).
(defun neoteric-process-tail (input-stream prefix)
    (let* ((c (peek-char nil input-stream nil my-eof-marker)))
      (cond
        ((eq c my-eof-marker) prefix)
        ((eql c #\( ) ; Implement f(x).
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
              (cons prefix (my-read-delimited-list #\) input-stream))))
        ((eql c #\[ )  ; Implement f[x]
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
                (cons '$bracket-apply$
                  (cons prefix
                    (my-read-delimited-list #\] input-stream)))))
        ((eql c #\{ )  ; Implement f{x}.
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
            (let ((tail (process-curly
                          (my-read-delimited-list #\} input-stream))))
              (if (null tail)
                (list prefix) ; Map f{} to (f), not (f ()).
                (list prefix tail)))))
        (t prefix))))


;;; Dispatch procedures.

; Read until }, then process list as infix list.
(defun neoteric-curly-brace (stream char)
  (declare (ignore char)) ; {
  (let ((result (my-read-delimited-list #\} stream)))
    (neoteric-process-tail stream (process-curly result))))

; Read preserving whitespace using the underlying readtable, then
; apply a neoteric tail if necessary.  This is necessary for handling
; various situations (including constituent characters)
; to ensure that trailing whitespace is NEVER consumed before looking for
; the tail.  Otherwise '{a + {b * c}} will be incorrectly
; interpreted as (A (+ (* B C))) instead of the correct (+ A (* B C)).
; That's because if the whitespace after "+" is (incorrectly)
; consumed, it will be interpreted as '{a +{b * c}}.

(defun wrap-read-n-tail (stream char)
  (unread-char char stream)
  (let ((saved-readtable *readtable*))
    (setq *readtable* *neoteric-underlying-readtable*)
    ; Do NOT make recursive, or spaces after atoms will be consumed.
    (let ((atom (read-preserving-whitespace stream t nil)))
      (setq *readtable* saved-readtable)
      (neoteric-process-tail stream atom))))

(defun wrap-continue (stream char)
  ; Call routine from original readtable, without removing, and
  ; invoke neoteric-process-tail.
  (neoteric-process-tail stream
    (funcall
      (get-macro-character char *neoteric-underlying-readtable*)
      stream char)))

(defun wrap-dispatch-tail (stream sub-char int)
  ; Call routine from original readtable, but leave our readtable in place,
  ; and invoke neoteric-process-tail.
  (neoteric-process-tail stream
    (funcall
      (get-dispatch-macro-character #\# sub-char
                                    *neoteric-underlying-readtable*)
      stream sub-char int)))

(defun wrap-dispatch-disabled-tail (stream sub-char int)
  ; Call routine from original readtable and disable temporarily our
  ; readtable.  Then invoke neoteric-process-tail.

  ; This is more convoluted than you'd expect, because
  ; Common Lisp's "read" provides no simple way to *prevent*
  ; consuming trailing whitespace if the read is at the top level.
  ; When that happens, trailing whitespace will be consumed *BEFORE* the
  ; neoteric-tail check is performed.  If, after the whitespace, there's
  ; something that looks like a tail, the wrong result will occur.
  ; E.G., the neoteric expression:
  ;   '#B101 (quote x)
  ; would in the "obvious" implementation be the incorrect: (5 |QUOTE| |X|)
  ; instead of the correct 5 followed by x.
  ; To deal with this, we collect all the characters before a delimiter,
  ; put them into a string, and read from the string instead.
  (neoteric-process-tail stream
    (let* ((ctext (string-before-delimiter stream ""))
           (*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (with-input-from-string (string-stream ctext)
        (funcall
          (get-dispatch-macro-character #\# sub-char
                                      *neoteric-underlying-readtable*)
          string-stream sub-char int)))))

(defun wrap-dispatch-disabled-notail (stream sub-char int)
  ; Call routine from original readtable and disable temporarily our
  ; readtable.  Do NOT invoke neoteric-process-tail.
  ; There's no obvious way to *prevent* this from consuming
  ; trailing whitespace if the top-level routine consumed trailing whitespace.
    (let ((*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (funcall
        (get-dispatch-macro-character #\# sub-char
                                      *neoteric-underlying-readtable*)
        stream sub-char int)))

(defun wrap-dispatch-special-read-tail (stream sub-char int)
  ; Get chars until a delimiter, then read it by disabling temporarily our
  ; readtable.  Then invoke neoteric-process-tail.
  (declare (ignore int))
  (unread-char sub-char stream)
  (neoteric-process-tail stream
    (let ((*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (my-read-to-delimiter stream "#"))))

(defun wrap-paren (stream char)
  (neoteric-process-tail stream
    (my-read-delimited-list ; (
      (if (eql char #\[) #\] #\) )
      stream)))


;;; Enablers

; Should be able to do this without eval-when, look at cmu-infix
(defmacro create-readtable ()
 `(defreadtable readable:neoteric
    (:merge :standard)
    ; Wrap character pairs.
    (:macro-char #\{ #'neoteric-curly-brace)
    (:macro-char #\} (get-macro-character #\)) nil)
    (:macro-char #\] (get-macro-character #\)) nil)
    (:macro-char #\[ #'wrap-paren nil)
    (:macro-char #\( #'wrap-paren nil)
    ; Wrap all constituents.  Presume ASCII for now.
    ; TODO: Don't wrap if they aren't constituents any more.
    ; append is used so symbols starting with an escape will work:
    ,@(mapcar (lambda (x) `(:macro-char ,x #'wrap-read-n-tail nil))
		(append *constituents* '(#\\ #\| #\")))
    ; Now deal with dispatch macro char; we'll just deal with default "#".
    ; set-dispatch-macro-character disp-char sub-char function
    ;                              &optional readtable
    ;    Where "function" takes parameters (stream char arg).
    ; get-dispatch-macro-character disp-char sub-char &optional readtable
    ; See: http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html
    ;
    ; How we wrap it depends on what will follow the macro char construct:
    ; - Datums-to-follow like #+ and #;.  No change; the default
    ;   neoteric readtable already handles datums.
    ; - Undefined or "signals error" - no change.
    ; - "Special-meaning" like #x.  These aren't followed by a datum. Instead,
    ;   this is a sequence of characters that represents some special value.
    ;   These characters (including the characters that started them)
    ;   are read until a delimiter, put in a string, and read from the string.
    ;   The result is then processed specially to look for
    ;   a neoteric tail.  That way, constructs like "#xa(#xb)" work and are
    ;   distinguished from  "#xa (#xb)". Use #'wrap-dispatch-disabled-tail.
    ;
    ; See Common Lisp hyperspec section 2.4.8
    ; http://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
    ; Below is every standard # macro character syntax (except undefined
    ; and signals error), in order (note the "debatable" ones):

    ;   ##      = reference to #= label    - Intentionally not wrapped
    ;   #'      = function abbreviation    - Intentionally not wrapped
    ;   #(...)  = vector                   - Intentionally not wrapped
    ;   #*      = bit-vector               - Special-meaning, wrapped
    ;   #,      = (was) load-time eval [Steele] - Intentionally not wrapped
    ;   #0..9   = used for infix arguments - Can't really wrap anyway.
    ;   #:      = uninterned symbol        - Special-meaning, wrapped
    ;   #;      = datum comment (extension)- Intentionally not wrapped
    ;   #=      = label following object   - Intentionally not wrapped
    ;   #|...|# = balanced comment         - Intentionally not wrapped
    ;   #+      = read-time conditional    - Intentionally not wrapped
    ;   #-      = read-time conditional    - Intentionally not wrapped
    ;   #.      = read-time evaluation     - Intentionally not wrapped
    ;   #A,#a   = array                    - Not currently wrapped (debatable).
    ;   #B,#b   = binary rational          - Special-meaning, wrapped
    ;   #C,#c   = complex number           - Not currently wrapped (debatable).
    ;             Complex numbers, because of their format, are tricky to wrap,
    ;             and there's no compelling reason to do so.
    ;   #O,#o   = octal rational           - Special-meaning, wrapped
    ;   #P,#p   = pathname                 - Not wrapped currently (debatable).
    ;             In the future this might be wrapped for #p"hi"(5), but
    ;             it's not obvious it would ever be used that way.
    ;   #R,#r   = radix-n rational         - Special-meaning, wrapped
    ;   #S,#s   = structure                - Not currently wrapped (debatable).
    ;   #X,#x   = hexadecimal rational     - Special-meaning, wrapped
    (:dispatch-macro-char #\# #\* #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\: #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\B #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\b #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\O #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\o #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\R #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\r #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\X #'wrap-dispatch-disabled-tail)
    (:dispatch-macro-char #\# #\x #'wrap-dispatch-disabled-tail)
    ;   #\char  = character object         - Special-meaning, wrapped
    (:dispatch-macro-char #\# #\\ #'wrap-dispatch-special-read-tail)))
  
(create-readtable)
(defun enable-neoteric-real ()
  (in-readtable neoteric))


; Read until }, then process list as infix list.
(defun full-curly-brace-infix-reader (stream char)
  (declare (ignore char))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-neoteric-real)
    (let* ((result (my-read-delimited-list #\} stream))
           (processed-result (process-curly result)))
      processed-result)))

(defreadtable readable:full-curly-infix
  (:merge :standard)
  ; Invoke curly-brace-infix-reader when opening curly brace is read in:
  (:macro-char #\{ #'full-curly-brace-infix-reader)
  ; This is necessary, else a cuddled closing brace will be part of an atom:
  (:macro-char #\} (get-macro-character #\) nil)))

(defun enable-full-curly-infix-real ()
  (in-readtable full-curly-infix)) 

(defun curly-infix-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-full-curly-infix-real)
    (read stream)))

(defun neoteric-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-neoteric-real)
    (read stream)))


;   (defun neoteric-filter ()
;     (handler-case
;       (do ((result (neoteric-read) (neoteric-read)))
;         (nil nil)
;         (write result)
;         (terpri))
;       (end-of-file ())))
;
;   (defun neoteric-load (filename)
;    (handler-case
;     (with-open-file (s (make-pathname :name filename) :direction :input)
;       (do ((result (neoteric-read s) (neoteric-read s)))
;         (nil nil)
;         (eval result)))
;     (end-of-file () )))


; TODO: Add writers, e.g., neoteric-write.


;;; Originally from "Common Lisp the Language, 2nd Edition" (Appendix C)
;;; by Guy L. Steele Jr.
;;; http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html

;;; The following are unique tokens used during processing. 
;;; They need not be symbols; they need not even be atoms.

(defvar *comma* (make-symbol "COMMA")) 
(defvar *comma-atsign* (make-symbol "COMMA-ATSIGN")) 
(defvar *comma-dot* (make-symbol "COMMA-DOT")) 
(defvar *bq-list* (make-symbol "BQ-LIST")) 
(defvar *bq-append* (make-symbol "BQ-APPEND")) 
(defvar *bq-list** (make-symbol "BQ-LIST*")) 
(defvar *bq-nconc* (make-symbol "BQ-NCONC")) 
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE")) 
(defvar *bq-quote* (make-symbol "BQ-QUOTE")) 
(defvar *bq-quote-nil* (list *bq-quote* nil))

;;; $ is pseudo-backquote and % is pseudo-comma.  This makes it 
;;; possible to test this code without interfering with normal 
;;; Common Lisp syntax.
;;; Reader macro characters: 
;;;    $foo is read in as (BACKQUOTE foo) 
;;;    %foo is read in as (#:COMMA foo) 
;;;    %@foo is read in as (#:COMMA-ATSIGN foo) 
;;;    %.foo is read in as (#:COMMA-DOT foo) 
;;; where #:COMMA is the value of the variable *COMMA*, etc.

;;; BACKQUOTE is an ordinary macro (not a read-macro) that 
;;; processes the expression foo, looking for occurrences of 
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT.  It constructs code 
;;; in strict accordance with the rules on pages 349-350 of 
;;; the first edition (pages 528-529 of this second edition). 
;;; It then optionally applies a code simplifier.

;; Original code that set read macro characters:

; (set-macro-character #\$ 
;   #'(lambda (stream char) 
;       (declare (ignore char)) 
;       (list 'backquote (read stream t nil t))))
; 
; (set-macro-character #\% 
;   #'(lambda (stream char) 
;       (declare (ignore char)) 
;         (case (peek-char nil stream t nil t) 
;           (#\@ (read-char stream t nil t) 
;                (list *comma-atsign* (read stream t nil t))) 
;           (#\. (read-char stream t nil t) 
;                (list *comma-dot* (read stream t nil t))) 
;           (otherwise (list *comma* (read stream t nil t))))))

 
;;; If the value of *BQ-SIMPLIFY* is non-NIL, then BACKQUOTE 
;;; processing applies the code simplifier.  If the value is NIL, 
;;; then the code resulting from BACKQUOTE is exactly that 
;;; specified by the official rules.

(defparameter *bq-simplify* t)

(defmacro backquote (x) 
  (bq-completely-process x))

;;; Backquote processing proceeds in three stages: 
;;; 
;;; (1) BQ-PROCESS applies the rules to remove occurrences of 
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT corresponding to 
;;; this level of BACKQUOTE.  (It also causes embedded calls to 
;;; BACKQUOTE to be expanded so that nesting is properly handled.) 
;;; Code is produced that is expressed in terms of functions 
;;; #:BQ-LIST, #:BQ-APPEND, and #:BQ-CLOBBERABLE.  This is done 
;;; so that the simplifier will simplify only list construction 
;;; functions actually generated by BACKQUOTE and will not involve 
;;; any user code in the simplification.  #:BQ-LIST means LIST, 
;;; #:BQ-APPEND means APPEND, and #:BQ-CLOBBERABLE means IDENTITY 
;;; but indicates places where "%." was used and where NCONC may 
;;; therefore be introduced by the simplifier for efficiency. 
;;; 
;;; (2) BQ-SIMPLIFY, if used, rewrites the code produced by 
;;; BQ-PROCESS to produce equivalent but faster code.  The 
;;; additional functions #:BQ-LIST* and #:BQ-NCONC may be 
;;; introduced into the code. 
;;; 
;;; (3) BQ-REMOVE-TOKENS goes through the code and replaces 
;;; #:BQ-LIST with LIST, #:BQ-APPEND with APPEND, and so on. 
;;; #:BQ-CLOBBERABLE is simply eliminated (a call to it being 
;;; replaced by its argument).  #:BQ-LIST* is replaced by either 
;;; LIST* or CONS (the latter is used in the two-argument case, 
;;; purely to make the resulting code a tad more readable).

(defun bq-completely-process (x) 
  (let ((raw-result (bq-process x))) 
    (bq-remove-tokens (if *bq-simplify* 
                          (bq-simplify raw-result) 
                          raw-result))))

(defun bq-process (x) 
  (cond ((atom x) 
         (list *bq-quote* x)) 
        ((eq (car x) 'backquote) 
         (bq-process (bq-completely-process (cadr x)))) 
        ((eq (car x) *comma*) (cadr x)) 
        ((eq (car x) *comma-atsign*) 
         (error ",@~S after `" (cadr x))) 
        ((eq (car x) *comma-dot*) 
         (error ",.~S after `" (cadr x))) 
        (t (do ((p x (cdr p)) 
                (q '() (cons (bracket (car p)) q))) 
               ((atom p) 
                (cons *bq-append* 
                      (nreconc q (list (list *bq-quote* p))))) 
             (when (eq (car p) *comma*) 
               (unless (null (cddr p)) (error "Malformed ,~S" p)) 
               (return (cons *bq-append* 
                             (nreconc q (list (cadr p)))))) 
             (when (eq (car p) *comma-atsign*) 
               (error "Dotted ,@~S" p)) 
             (when (eq (car p) *comma-dot*) 
               (error "Dotted ,.~S" p))))))

;;; This implements the bracket operator of the formal rules.

(defun bracket (x) 
  (cond ((atom x) 
         (list *bq-list* (bq-process x))) 
        ((eq (car x) *comma*) 
         (list *bq-list* (cadr x))) 
        ((eq (car x) *comma-atsign*) 
         (cadr x)) 
        ((eq (car x) *comma-dot*) 
         (list *bq-clobberable* (cadr x))) 
        (t (list *bq-list* (bq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra 
;;; purposes: (1) it handles dotted lists; (2) it tries to make 
;;; the result share with the argument x as much as possible.

(defun maptree (fn x) 
  (if (atom x) 
      (funcall fn x) 
      (let ((a (funcall fn (car x))) 
            (d (maptree fn (cdr x)))) 
        (if (and (eql a (car x)) (eql d (cdr x))) 
            x 
            (cons a d)))))

;;; This predicate is true of a form that when read looked 
;;; like %@foo or %.foo.

(defun bq-splicing-frob (x) 
  (and (consp x) 
       (or (eq (car x) *comma-atsign*) 
           (eq (car x) *comma-dot*))))

 
;;; This predicate is true of a form that when read 
;;; looked like %@foo or %.foo or just plain %foo.

(defun bq-frob (x) 
  (and (consp x) 
       (or (eq (car x) *comma*) 
           (eq (car x) *comma-atsign*) 
           (eq (car x) *comma-dot*))))

;;; The simplifier essentially looks for calls to #:BQ-APPEND and 
;;; tries to simplify them.  The arguments to #:BQ-APPEND are 
;;; processed from right to left, building up a replacement form. 
;;; At each step a number of special cases are handled that, 
;;; loosely speaking, look like this: 
;;; 
;;;  (APPEND (LIST a b c) foo) => (LIST* a b c foo) 
;;;       provided a, b, c are not splicing frobs 
;;;  (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo)) 
;;;       provided a, b, c are not splicing frobs 
;;;  (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo) 
;;;  (APPEND (CLOBBERABLE x) foo) => (NCONC x foo)

(defun bq-simplify (x) 
  (if (atom x) 
      x 
      (let ((x (if (eq (car x) *bq-quote*) 
                   x 
                   (maptree #'bq-simplify x)))) 
        (if (not (eq (car x) *bq-append*)) 
            x 
            (bq-simplify-args x)))))

(defun bq-simplify-args (x) 
  (do ((args (reverse (cdr x)) (cdr args)) 
       (result 
         nil 
         (cond ((atom (car args)) 
                (bq-attach-append *bq-append* (car args) result)) 
               ((and (eq (caar args) *bq-list*) 
                     (notany #'bq-splicing-frob (cdar args))) 
                (bq-attach-conses (cdar args) result)) 
               ((and (eq (caar args) *bq-list**) 
                     (notany #'bq-splicing-frob (cdar args))) 
                (bq-attach-conses 
                  (reverse (cdr (reverse (cdar args)))) 
                  (bq-attach-append *bq-append* 
                                    (car (last (car args))) 
                                    result))) 
               ((and (eq (caar args) *bq-quote*) 
                     (consp (cadar args)) 
                     (not (bq-frob (cadar args))) 
                     (null (cddar args))) 
                (bq-attach-conses (list (list *bq-quote* 
                                              (caadar args))) 
                                  result)) 
               ((eq (caar args) *bq-clobberable*) 
                (bq-attach-append *bq-nconc* (cadar args) result)) 
               (t (bq-attach-append *bq-append* 
                                    (car args) 
                                    result))))) 
      ((null args) result)))

(defun null-or-quoted (x) 
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND 
;;; or #:BQ-NCONC.  This produces a form (op item result) but 
;;; some simplifications are done on the fly: 
;;; 
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g) 
;;;  (op item 'nil) => item, provided item is not a splicable frob 
;;;  (op item 'nil) => (op item), if item is a splicable frob 
;;;  (op item (op a b c)) => (op item a b c)

(defun bq-attach-append (op item result) 
  (cond ((and (null-or-quoted item) (null-or-quoted result)) 
         (list *bq-quote* (append (cadr item) (cadr result)))) 
        ((or (null result) (equal result *bq-quote-nil*)) 
         (if (bq-splicing-frob item) (list op item) item)) 
        ((and (consp result) (eq (car result) op)) 
         (list* (car result) item (cdr result))) 
        (t (list op item result))))

;;; The effect of BQ-ATTACH-CONSES is to produce a form as if by 
;;; `(LIST* ,@items ,result) but some simplifications are done 
;;; on the fly. 
;;; 
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d) 
;;;  (LIST* a b c 'nil) => (LIST a b c) 
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g) 
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)

(defun bq-attach-conses (items result) 
  (cond ((and (every #'null-or-quoted items) 
              (null-or-quoted result)) 
         (list *bq-quote* 
               (append (mapcar #'cadr items) (cadr result)))) 
        ((or (null result) (equal result *bq-quote-nil*)) 
         (cons *bq-list* items)) 
        ((and (consp result) 
              (or (eq (car result) *bq-list*) 
                  (eq (car result) *bq-list**))) 
         (cons (car result) (append items (cdr result)))) 
        (t (cons *bq-list** (append items (list result))))))

;;; Removes funny tokens and changes (#:BQ-LIST* a b) into 
;;; (CONS a b) instead of (LIST* a b), purely for readability.

(defun bq-remove-tokens (x) 
  (cond ((eq x *bq-list*) 'list) 
        ((eq x *bq-append*) 'append) 
        ((eq x *bq-nconc*) 'nconc) 
        ((eq x *bq-list**) 'list*) 
        ((eq x *bq-quote*) 'quote) 
        ((atom x) x) 
        ((eq (car x) *bq-clobberable*) 
         (bq-remove-tokens (cadr x))) 
        ((and (eq (car x) *bq-list**) 
              (consp (cddr x)) 
              (null (cdddr x))) 
         (cons 'cons (maptree #'bq-remove-tokens (cdr x)))) 
        (t (maptree #'bq-remove-tokens x))))


;;; sweet.cl
;;; Implements sweet-expressions from the "readable" approach for Lisp.

;;; Copyright (C) 2007-2014 by David A. Wheeler
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.


; We can't portably overide "read" directly, we have to manipulate
; the readtable to implement sweet-expressions.
; This readtable basically redirects EVERY character to a specific procedure,
; effectively taking over "read":
(defvar *sweet-readtable*
  "This table redirects any input to sweet-expression processing")

; The underlying readtable is mostly a neoteric reader.  However,
; we must implement a slightly different underlying reader that
; reads #|...|# and #;datum. The problem is that if the underlying reader
; return no values, e.g., "(values)" - the Common Lisp
; "read" will instantly recurse *outside* of our control to read the next
; datum.  That's the wrong thing to do, because that no-values item might
; be the only thing on the line, and in that case it should
; operate as a placeholder for that indentation position.
; Thus, we'll specially wrap such cases and return a
; distinctive value "no-neoteric-value", to represent the "no value" case.
(defvar *underlying-sweet-readtable*
  "This table is basically neoteric-expressions with some tweaks")

; Wrapping all character codes up to char-code-limit doesn't really work
; correctly.  This is the max char code that will be wrapped by readable's
; front readtable.
(defvar *my-char-code-limit* 255)

; These stubs could be used to attach position info
(defun attach-sourceinfo (pos value)
  (declare (ignore pos))
  value)
(defun get-sourceinfo (stream)
  (declare (ignore stream))
  nil)

(defun eof-objectp (c) (eq c my-eof-marker))

(defun string-length (s) (length s))

; Note: Use define-constant to work around SBCL problem.

(define-constant period-symbol '|.|)

(define-constant scomment-result '(scomment ()))

; Marker for empty values.
; Several Common Lisp readtable constructs return nothing using (values),
; but unfortunately when that happens the build-in Common Lisp reader
; performs actions that we can't intercept.  So we'll override the "empty"
; returns with this instead, so that they will get passed back to us
; and allow us to override the reader.
(define-constant no-neoteric-value (make-symbol "no-neoteric-value"))

; Represent no value at all, in the sweet-expression processing.
(define-constant empty-value (make-symbol "empty-value"))

(define-constant datum-commentw-tag (make-symbol "datum-commentw"))

(define-constant vertical-tab (code-char 11)) ; VT is decimal 11.
(define-constant form-feed #\page)            ; FF is decimal 12.

(define-constant whitespace-chars
   (list #\space #\tab #\linefeed #\newline #\return vertical-tab form-feed))

(defun my-char-whitespacep (c)
  (member c whitespace-chars))

(define-constant line-ending-chars (list #\newline #\linefeed #\return))
(defun char-line-endingp (char)
  (or
    (eof-objectp char)
    (member char line-ending-chars)))

; Does character "c" begin a line comment (;) or end-of-line?
(define-constant initial-comment-eol '(#\; #\newline #\linefeed #\return))
(defun lcomment-eolp (c)
  (member c initial-comment-eol))

(defun my-peek-char (stream)
  (let ((c (peek-char nil stream nil my-eof-marker)))
    ; (format t "DEBUG: my-peek-char: ~@C~%" c)
    c))
(defun my-read-char (stream)
  (let ((c (read-char stream t nil nil)))
    ; (format t "DEBUG: my-read-char: ~@C~%" c)
    c))


; Consume an end-of-line sequence, ('\r' '\n'? | '\n')
(defun consume-end-of-line (stream)
  (let ((c (my-peek-char stream)))
    (cond
      ((eof-objectp c) (values))
      ((eql c #\return)
        (my-read-char stream)
        (if (eql (my-peek-char stream) #\linefeed)
            (my-read-char stream)))
      ((or (eql c #\linefeed) (eql c #\newline))
        (my-read-char stream)))))

; Consume every non-eol character in the current line.
; End on EOF or end-of-line char.
; Do NOT consume the end-of-line character(s).
(defun consume-to-eol (stream)
  (let ((c (my-peek-char stream)))
    (when (not (char-line-endingp c))
        (my-read-char stream)
        (consume-to-eol stream))))

; detect #| or |#
(define-constant hash-pipe-comment-nestsp t)
(defun nest-comment (stream)
  (let ((c (my-read-char stream)))
    (cond
      ((eof-objectp c)
        no-neoteric-value)
      ((char= c #\|)
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\#))
              (progn
                (my-read-char stream)
                no-neoteric-value)
              (nest-comment stream))))
      ((and hash-pipe-comment-nestsp (char= c #\#))
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\|))
              (progn
                (my-read-char stream)
                (nest-comment stream))
              no-neoteric-value)
          (nest-comment stream)))
      (t
        (nest-comment stream)))))

; Implement #|...|#
(defun wrap-comment-block (stream sub-char int)
  (declare (ignore sub-char int))
  (nest-comment stream))

; Implement #;datum
(defun wrap-comment-datum (stream sub-char int)
  (declare (ignore sub-char int))
  (if (my-char-whitespacep (my-peek-char stream))
    datum-commentw-tag
    (let ((junk (neoteric-read-nocomment stream)))
      (declare (ignore junk))
      no-neoteric-value)))

; There is no standard mechanism to unread multiple characters.
; Therefore, the key productions and some of their supporting procedures
; return both the information on what ended their reading process,
; as well the actual value (if any) they read before whatever stopped them.
; That way, procedures can process the value as read, and then pass on
; the ending information to whatever needs it next.  This approach,
; which we call a "non-tokenizing" implementation, implements a tokenizer
; via procedure calls instead of needing a separate tokenizer.
; The ending information can be:
; - "stopper" - this is returned by productions etc. that do NOT
;     read past the of a line (outside of paired characters and strings).
;     It is 'normal if it ended normally (e.g., at end of line); else it's
;     'sublist-marker ($), 'group-split-marker (\\), 'collecting (<*),
;     'collecting-end (*>), 'scomment (special comments like #|...|#),
;     'datum-commentw, or
;     'abbrevw (initial abbreviation with whitespace after it).
; - "new-indent" - this is returned by productions etc. that DO read
;     past the end of a line.  Such productions typically read the
;     next line's indent to determine if they should return.
;     If they should, they return the new indent so callers can
;     determine what to do next.  A "*>" should return even though its
;     visible indent level is length 0; we handle this by prepending
;     all normal indents with "^", and "*>" generates a length-0 indent
;     (which is thus shorter than even an indent of 0 characters).

; Define let-splitter macro to simplify common code pattern.
(defmacro let-splitter ((full first-value second-value) expr &rest body)
  `(let* ((,full ,expr)
          (,first-value (car ,full))
          (,second-value (cadr ,full)))
         ,@body))

(define-constant group-split '\\)
(define-constant group-split-char #\\ ) ; First character of split symbol.

(defvar non-whitespace-indent #\!) ; Non-whitespace-indent char.

(define-constant sublist '$)
(define-constant sublist-char #\$) ; First character of sublist symbol.


(defun indentation>p (indentation1 indentation2)
  (let ((len1 (string-length indentation1))
        (len2 (string-length indentation2)))
    (and (> len1 len2)
         (string= indentation2 (subseq indentation1 0 len2)))))

(defun indentation>=p (indentation1 indentation2)
  (or
    (string= indentation1 indentation2)
    (indentation>p indentation1 indentation2)))

; Return t if char is space or tab.
(declaim (inline char-hspacep))
(defun char-hspacep (char)
  (or (eql char #\space)
      (eql char #\tab)))

; Consume 0+ spaces or tabs.  Uses direct iteraction, not tail recursion.
(defun hspaces (stream)
  (loop
     while (char-hspacep (my-peek-char stream))
     do (my-read-char stream)))

; Return t if char is space, tab, or !
(declaim (inline char-icharp))
(defun char-icharp (char)
  (or (eql char #\space)
      (eql char #\tab)
      (eql char non-whitespace-indent)))

(declaim (inline accumulate-ichar))
(defun accumulate-ichar (stream)
  (loop while (char-icharp (my-peek-char stream))
        collect (my-read-char stream)))

(defun consume-ff-vt (stream)
  (let ((c (my-peek-char stream)))
    (cond
      ((or (eql c form-feed) (eql c vertical-tab))
        (my-read-char stream)
        (consume-ff-vt stream)))))

; Do 2-item append, but report read-error if the LHS is not a proper list.
; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
(defun my-append (lhs rhs)
  (cond
    ((eq lhs empty-value) rhs)
    ((eq rhs empty-value) lhs)
    ((listp lhs) (append lhs rhs))
    (t
      (read-error "Must have proper list on left-hand-side to append data"))))

; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
; else returns ('normal n-expr).
(defun n-expr-or-scomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
    (cond
      ((eq result no-neoteric-value) scomment-result)
      ((eq result datum-commentw-tag) '(datum-commentw ()))
      (t (list 'normal result)))))

; Read a straight-up n-expression.  Skip scomments.
(defun neoteric-read-nocomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
    (cond
      ((eq result no-neoteric-value) (neoteric-read-nocomment stream))
      ((eq result datum-commentw-tag)
        (neoteric-read-nocomment stream) ; Consume the next n-expression.
        (neoteric-read-nocomment stream))
      (t result))))

; Read an n-expression.  Returns ('normal n-expr) in most cases;
; if it's a special marker, the car is the marker name instead of 'normal.
; Markers only have special meaning if their first character is
; the "normal" character, e.g., {$} is not a sublist.
; Call "process-sharp" if first char is "#".
(defun n-expr (stream)
  (let ((c (my-peek-char stream)))
    (let-splitter (results type expr)
                  (n-expr-or-scomment stream)
      (declare (ignore type))
      ; (princ "DEBUG: n-expr, results=") (write results) (terpri)
      ; (princ "DEBUG: n-expr, car results=") (write (car results)) (terpri)
      ; (princ "DEBUG: n-expr, car results scomment=") (write (eq (car results) 'scomment)) (terpri)
      (if (eq (car results) 'scomment)
          results
          (cond
            ; TODO: Improve Workaround for symbol packaging:
            ((not (symbolp expr))
              results)
            ((and (eql c sublist-char) (string= (symbol-name expr) "$"))
              (list 'sublist-marker '()))
            ((and (eql c group-split-char) (string= (symbol-name expr) "\\"))
              (list 'group-split-marker '()))
            ((and (eql c #\<) (string= (symbol-name expr) "<*"))
              (list 'collecting '()))
            ((and (eql c #\*) (string= (symbol-name expr) "*>"))
              (list 'collecting-end '()))
            ((and (eql c #\$) (string= (symbol-name expr) "$$$"))
              (read-error "$$$ is reserved."))
            ((and (eql c #\.) (string= (symbol-name expr) "."))
              (list 'period-marker '()))
            (t
              results))))))

; Check if we have abbrev+whitespace.  If the current peeked character
; is one of certain whitespace chars,
; return 'abbrevw as the marker and abbrev-procedure
; as the value (the cadr). Otherwise, return ('normal n-expr).
; We do NOT consume the peeked char (so EOL can be examined later).
; Note that this calls the neoteric-read procedure directly, because
; quoted markers are no longer markers. E.G., '$ is just (quote $).
(defun maybe-initial-abbrev (stream abbrev-procedure)
  (let ((c (my-peek-char stream)))
    (if (or (char-hspacep c) (eql c #\return) (eql c #\linefeed)
            (eql c #\newline))
        (list 'abbrevw abbrev-procedure)
        (list 'normal
          (list abbrev-procedure (neoteric-read-nocomment stream))))))

; Read the first n-expr on a line; handle abbrev+whitespace specially.
; Returns ('normal VALUE) in most cases.
(defun n-expr-first (stream)
  (case (my-peek-char stream)
    ((#\') 
      (my-read-char stream)
      (maybe-initial-abbrev stream 'quote))
    ((#\`) 
      (my-read-char stream)
      (maybe-initial-abbrev stream 'backquote))
    ((#\,) 
      (my-read-char stream)
      (case (my-peek-char stream)
        (#\@
          (my-read-char stream)
          (maybe-initial-abbrev stream *comma-atsign*))
        (#\.
          (my-read-char stream)
          (maybe-initial-abbrev stream *comma-dot*))
        (otherwise
          (maybe-initial-abbrev stream *comma*))))
    (t
      (n-expr stream))))

; Consume ;-comment (if there), consume EOL, and return new indent.
; Skip ;-comment-only lines; a following indent-only line is empty.
(defun get-next-indent (stream)
  (consume-to-eol stream)
  (consume-end-of-line stream)
  (let* ((indentation-as-list (cons #\^ (accumulate-ichar stream)))
         (c (my-peek-char stream)))
    (cond
      ((eof-objectp c) "^") ; EOF ; end any existing expression.
      ((eql c #\;)  ; A ;-only line, consume and try again.
        (get-next-indent stream))
      ((lcomment-eolp c) ; Indent-only line
        (if (member #\! indentation-as-list)
            (get-next-indent stream)
            "^"))
      (t (concatenate 'string indentation-as-list)))))

; Read and throw away "skippable" content (text that is semantically the
; same as whitespace but is complex).  This implements the sequence:
;   (scomment hs | datum-commentw hs n-expr hs)
(defun skippable (stopper stream)
  (cond
  ((eq stopper 'scomment)
    (hspaces stream))
  ((eq stopper 'datum-commentw)
    (hspaces stream)
    (if (not (lcomment-eolp (my-peek-char stream)))
      (progn
        (n-expr stream)
        (hspaces stream))
      (read-error "Datum comment start not followed a datum (EOL instead)")))
  (t (read-error "skippable: Impossible case"))))

; Utility declarations and functions

(defun conse (x y) ; cons, but handle "empty" values
  (cond
    ((eq y empty-value) x)
    ((eq x empty-value) y)
    (t (cons x y))))

(defun appende (x y) ; append, but handle "empty" values
  (cond
    ((eq y empty-value) x)
    ((eq x empty-value) y)
    (t (append x y))))

(defun list1e (x) ; list, but handle "empty" values
  (if (eq x empty-value)
      '()
      (list x)))

(defun list2e (x y) ; list, but handle "empty" values
  (if (eq x empty-value)
      y
      (if (eq y empty-value)
         x
         (list x y))))

; If x is a 1-element list, return (car x), else return x
(defun monify (x)
  (cond
    ((atom x) x)
    ((null (cdr x)) (car x))
    (t x)))


; Read the contents of a collecting list and return it.
; Precondition: Have already read collecting start and horizontal spaces.
; Postcondition: Consumed the matching COLLECTING_END.
; Return contents (value) of collecting-content.  It does *not* report a
; stopper or ending indent, because it is *ONLY* stopped by collecting-end
(defun collecting-content (stream)
  (let* ((c (my-peek-char stream)))
    (cond
      ((eof-objectp c)
       (read-error "Collecting tail: EOF before collecting list ended."))
      ((lcomment-eolp c)
        (consume-to-eol stream)
        (consume-end-of-line stream)
        (collecting-content stream))
      ((char-icharp c)
        (let* ((indentation (accumulate-ichar stream))
               (c (my-peek-char stream)))
          (declare (ignore indentation))
          (if (lcomment-eolp c)
              (collecting-content stream)
              (read-error "Collecting tail: Only ; after indent."))))
      ((or (eql c form-feed) (eql c vertical-tab))
        (consume-ff-vt stream)
        (if (lcomment-eolp (my-peek-char stream))
            (collecting-content stream)
            (read-error "Collecting tail: FF and VT must be alone on line.")))
      (t
        (let-splitter (it-full-results it-new-indent it-value)
                      (it-expr stream "^")
          (cond
            ((string= it-new-indent "")
              ; Specially compensate for "*>" at the end of a line if it's
              ; after something else.  This must be interpreted as EOL *>,
              ; which would cons a () after the result.
              ; Directly calling list for a non-null it-value has
              ; the same effect, but is a lot quicker and simpler.
              (cond
                ((null it-value) it-value)
                ((eq it-value empty-value) '())
                (t (list it-value))))
            (t (conse it-value (collecting-content stream)))))))))
; Read the first n-expr on a line; handle abbrev+whitespace specially.
; Returns ('normal VALUE) in most cases.
(defun n-expr-error (stream full)
  (if (not (eq (car full) 'normal))
      (read-error "BUG! n-expr-error called but stopper not normal."))
  (if (lcomment-eolp (my-peek-char stream))
      full ; All done!
      (let-splitter (n-full-results n-stopper n-value)
                    (n-expr stream)
        (declare (ignore n-value))
        (cond
          ((or (eq n-stopper 'scomment) (eq n-stopper 'datum-commentw))
            (skippable n-stopper stream)
            (n-expr-error stream full))
          ((eq n-stopper 'normal)
            (read-error "Illegal second value after '.'."))
          (t ; We found a stopper, return it with the value from "full"
            (list n-stopper (cadr full)))))))

; Read input after a lone ".", normally exactly one datum.
; Returns (stopper value-after-period)
(defun post-period (stream)
  (if (not (lcomment-eolp (my-peek-char stream)))
      (let-splitter (pn-full-results pn-stopper pn-value)
                    (n-expr stream)
        (declare (ignore pn-value))
        (cond
          ((or (eq pn-stopper 'scomment) (eq pn-stopper 'datum-commentw))
            (skippable pn-stopper stream)
            (post-period stream))
          ((eq pn-stopper 'normal)
            (hspaces stream)
            (n-expr-error stream pn-full-results))
          ((eq pn-stopper 'collecting)
            (hspaces stream)
            (let ((cl (collecting-content stream)))
              (hspaces stream)
              (n-expr-error stream (list 'normal cl))))
          ((eq pn-stopper 'period-marker)
            (list 'normal period-symbol))
          (t ; Different stopper; respond as empty branch with that stopper
            (list pn-stopper (list period-symbol)))))
      (list 'normal period-symbol))) ; Empty branch.

; Read the 1+ n-expressions on one line, and return them as a list.
; If there is exactly one n-expression on the line,
; it returns a list of exactly one item.
; Precondition: At beginning of line after indent
; Postcondition: At unconsumed EOL
; Returns (stopper computed-value).
; The stopper may be 'normal, 'scomment (special comment),
; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
(defun line-exprs (stream indent)
  (let-splitter (basic-full-results basic-special basic-value)
                (n-expr-first stream)
    ; (princ "DEBUG: line-exprs=") (write basic-full-results) (terpri)
    (cond
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((cl-results (collecting-content stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let-splitter (rr-full-results rr-stopper rr-value)
                            (rest-of-line stream indent)
                (list rr-stopper (cons cl-results rr-value)))
              (list 'normal (list cl-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (let-splitter (cl-full-results cl-stopper cl-value)
                            (post-period stream)
                (list cl-stopper (list cl-value))))
            (list 'normal (list period-symbol))))
      ((not (eq basic-special 'normal)) basic-full-results)
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let-splitter (br-full-results br-stopper br-value)
                          (rest-of-line stream indent)
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t 
        (list 'normal (list basic-value))))))

; Read the rest of the expressions on a line,
; after the first expression of the line.  This supports line-exprs.
; Precondition: At beginning of non-first expression on line (past hspace)
; Postcondition: At unconsumed EOL
; Returns (stopper computed-value); stopper may be 'normal, etc.
; Read in one n-expr, then process based on whether or not it's special.
(defun rest-of-line (stream indent)
  (let-splitter (basic-full-results basic-special basic-value)
                (n-expr stream)
    (cond
      ((or (eq basic-special 'scomment) (eq basic-special 'datum-commentw))
        (skippable basic-special stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (rest-of-line stream indent)
            (list 'normal '())))
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((cl-results (collecting-content stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let-splitter (rr-full-results rr-stopper rr-value)
                            (rest-of-line stream indent)
                (list rr-stopper (cons cl-results rr-value)))
              (list 'normal (list cl-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (post-period stream))
            ; (list 'normal (list period-symbol)) ; To interpret as |.|
            (read-error "Cannot end line with '.'")))
      ((eq basic-special 'group-split-marker)
        ; Local extension - allow \\ as line-continuation, a
        ; capability useful in Common Lisp.
        ; This is *NOT* a SRFI-110 requirement!!
        (hspaces stream)
        (if (lcomment-eolp (my-peek-char stream))
          (let ((new-indent (get-next-indent stream)))
            (if (indentation>=p new-indent indent)
              (rest-of-line stream indent)
              (read-error "Line continuation indentation is inconsistent.")))
          (list basic-special '())))
      ((not (eq basic-special 'normal)) (list basic-special '()))
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let-splitter (br-full-results br-stopper br-value)
                          (rest-of-line stream indent)
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t (list 'normal (list basic-value))))))

; Read the sequence of 1+ child lines in an it_expr
; (e.g., after "line_expr"), each of which is itself an it_expr.
; It returns the list of expressions in the body and the new indent as
; (new-indent computed-value).
; We name this "read-body", not "body", to make this more similar to the
; Scheme implementation (which uses "read-body" to avoid an error in rscheme),
; and also to reduce confusion ("body" is used with other meanings elsewhere).
(defun read-body (stream starting-indent)
  (let-splitter (i-full-results i-new-indent i-value)
                (it-expr stream starting-indent)
    (if (string= starting-indent i-new-indent)
        (if (eq i-value period-symbol)
            (let-splitter (f-full-results f-new-indent f-value)
                          (it-expr stream i-new-indent)
              (if (not (indentation>p starting-indent f-new-indent))
                  (read-error "Dedent required after lone . and value line."))
              (list f-new-indent f-value)) ; final value of improper list
            (if (eq i-value empty-value)
                (read-body stream i-new-indent)
                (let-splitter (nxt-full-results nxt-new-indent nxt-value)
                              (read-body stream i-new-indent)
                  (list nxt-new-indent (conse i-value nxt-value)))))
        (list i-new-indent (list1e i-value))))) ; dedent - end list.

; Read a sweet-expression that doesn't have a special prefix.
; Returns (new-indent computed-value)
(defun it-expr-real (stream starting-indent)
  (let-splitter (line-full-results line-stopper line-value)
                (line-exprs stream starting-indent)
    ; (princ "DEBUG: it-expr-real: line-exprs result=") (write line-full-results) (terpri)
    (if (and (not (null line-value)) (not (eq line-stopper 'abbrevw)))
        ; Production line-exprs produced at least one n-expression:
        (cond
          ((eq line-stopper 'group-split-marker)
            (hspaces stream)
            ; This error can't happen due to \\ line continuation extension,
            ; but we will test it just in case:
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "Cannot follow split with end of line")
                (list starting-indent (monify line-value))))
          ((eq line-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after sublist."))
            (let-splitter (sub-i-full-results sub-i-new-indent sub-i-value)
                          (it-expr stream starting-indent)
              (list sub-i-new-indent
                (my-append line-value (list sub-i-value)))))
          ((eq line-stopper 'collecting-end)
            ; Note that indent is "", forcing dedent all the way out.
            (list ""
              (if (eq line-value empty-value)
                '()
                (monify line-value))))
          ((lcomment-eolp (my-peek-char stream))
            (let ((new-indent (get-next-indent stream)))
              (if (indentation>p new-indent starting-indent)
                  (let-splitter (body-full-results body-new-indent body-value)
                                (read-body stream new-indent)
                    (list body-new-indent (my-append line-value body-value)))
                  (list new-indent (monify line-value)))))
          ((eof-objectp (my-peek-char stream))
            (list " " (monify line-value)))
          (t
            (read-error "Unexpected text after n-expression")))
        ; Here, line-exprs begins with something special like GROUP-SPLIT:
        (cond
          ((eq line-stopper 'datum-commentw)
            (hspaces stream)
            (cond
              ((not (lcomment-eolp (my-peek-char stream)))
                (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                              (it-expr stream starting-indent)
                  (declare (ignore is-i-value))
                  (list is-i-new-indent empty-value)))
              (t
                (let ((new-indent (get-next-indent stream)))
                  (if (indentation>p new-indent starting-indent)
                    (let-splitter (body-full-results body-new-indent body-value)
                                  (read-body stream new-indent)
                      (declare (ignore body-value))
                      (list body-new-indent empty-value))
                    (read-error "#;+EOL must be followed by indent"))))))
          ((or (eq line-stopper 'group-split-marker)
               (eq line-stopper 'scomment))
            (hspaces stream)
            (if (not (lcomment-eolp (my-peek-char stream)))
                (it-expr stream starting-indent) ; Skip and try again.
                (let ((new-indent (get-next-indent stream)))
                  (cond
                    ((indentation>p new-indent starting-indent)
                      (read-body stream new-indent))
                    (t
                      (list new-indent empty-value))))))
          ((eq line-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after solo sublist."))
            (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                          (it-expr stream starting-indent)
              (list is-i-new-indent
                (list1e is-i-value))))
          ((eq line-stopper 'abbrevw)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (progn
                  (let ((new-indent (get-next-indent stream)))
                    (if (not (indentation>p new-indent starting-indent))
                        (read-error "Indent required after abbreviation."))
                    (let-splitter (ab-full-results ab-new-indent ab-value)
                                  (read-body stream new-indent)
                      (list ab-new-indent
                        (append (list line-value) ab-value)))))
                (let-splitter (ai-full-results ai-new-indent ai-value)
                              (it-expr stream starting-indent)
                  (list ai-new-indent
                    (list2e line-value ai-value)))))
          ((eq line-stopper 'collecting-end)
            (list "" line-value))
          (t 
            (read-error "Initial line-expression error."))))))

; Read it-expr.  This is a wrapper that attaches source info
; and checks for consistent indentation results, then calls it-expr-real.
(defun it-expr (stream starting-indent)
  (let ((pos (get-sourceinfo stream)))
    (let-splitter (results results-indent results-value)
                  (it-expr-real stream starting-indent)
      (if (indentation>p results-indent starting-indent)
          (read-error "Inconsistent indentation."))
      (list results-indent (attach-sourceinfo pos results-value)))))

 
; Read the rest of an initial-indent-expr (a sweet-expression with
; a special initial value).
(defun initial-indent-expr-tail (stream)
  (if (not (member (my-peek-char stream) initial-comment-eol))
      (let-splitter (results results-stopper results-value)
                    (n-expr-or-scomment stream)
        (cond
          ((member results-stopper '(scomment datum-commentw))
            (skippable results-stopper stream)
            (initial-indent-expr-tail stream))
          (t ; Normal n-expr, return one value.
            ; The following "if" is a work-around for a bug in clisp's REPL.
            ; Without it, in sequential initial-indent lines like this:
            ;   1 2 3
            ;   4 5 6
            ; the 2nd line's non-first n-expressions are skipped (e.g., 5 6).
            ; We work around this by consuming an EOL immediately following
            ; the last n-expression on an initial indent line.
            ; This work-around fails if the first line ends in space or tab,
            ; but clisp REPL users are highly unlikely to notice this.
            ; This work-around won't affect correctly-working systems
            ; because without this code, it'd just skip a blank EOL anyway.
            ; Correctly-working systems include clisp's file-execution code
            ; and a clisp loop of (write (read)), interestingly enough.
            (if (member (my-peek-char stream) initial-comment-eol)
              (my-read-char stream))
            results-value)))
      (progn
        (consume-to-eol stream)
        (consume-end-of-line stream)
        empty-value))) ; (t-expr-real stream)

; Read a sweet-expression (t-expression).  Handle special
; cases, such as initial indent; call it-expr for normal case.
(defun t-expr-real (stream)
  (let* ((c (my-peek-char stream)))
        (cond
          ((lcomment-eolp c)
            (consume-to-eol stream)
            (consume-end-of-line stream)
            (t-expr-real stream))
          ((or (eql c form-feed) (eql c vertical-tab))
            (consume-ff-vt stream)
            (if (not (lcomment-eolp (my-peek-char stream)))
              (read-error "FF and VT must be alone on line in a sweet-expr"))
            (t-expr-real stream))
          ((char-icharp c) ; initial-indent-expr
            (accumulate-ichar stream) ; consume and throw away ichars
            (initial-indent-expr-tail stream))
          (t
            (let-splitter (results results-indent results-value)
                          (it-expr stream "^")
              (if (string= results-indent "")
                  (read-error "Closing *> without preceding matching <*."))
              results-value)))))

; Top level - read a sweet-expression (t-expression).  Handle special values.
(defun t-expr (stream)
  (let* ((te (t-expr-real stream)))
    (if (eq te empty-value)
        (t-expr stream)
        te)))

; Transition to read a sweet-expression (t-expression).
; Lisp "read" tried to read a character, and got redirected here.
; We will unread that character, and then invoke our own reader.
(defun t-expr-entry (stream char)
  (unread-char char stream)
  ; (princ "DEBUG entry: ") (write char) (terpri)
  (let ((*readtable* *underlying-sweet-readtable*))
    (handler-case (t-expr stream)
      ; Specially handle EOF so the underlying reader will see it.
      (end-of-file () (values)))))



(eval
  `(defreadtable readable:sweet-readtable
     (:case (readtable-case *readtable*))
     (:syntax-from :standard #\# #\')
     ,@(loop for ci from 0 upto *my-char-code-limit*
	     collect `(:macro-char ,(code-char ci) #'t-expr-entry nil))
     ;(:fuse readable:sweet-underlying-readtable)
     ))

(defreadtable readable:sweet-underlying-readtable
    (:merge readable:neoteric)
    (:fuse readable:sweet-underlying-readtable) ; Could need to fuse sweet-readtable onto sweet-redirect-readtable
    (:dispatch-macro-char #\# #\| #'wrap-comment-block)
    (:dispatch-macro-char #\# #\; #'wrap-comment-datum)
    (:macro-char #\`
    #'(lambda (stream char)
	(declare (ignore char))
	(list 'backquote (my-read-datum stream))))
    (:macro-char #\`
    #'(lambda (stream char)
	(declare (ignore char))
	(case (my-peek-char stream)
	    (#\@ (my-read-char stream)
		(list *comma-atsign* (my-read-datum stream)))
	    (#\. (read-char stream t nil t)
		(list *comma-dot* (my-read-datum stream)))
	    (otherwise (list *comma* (my-read-datum stream)))))))



; Set up a readtable that'll redirect any character to t-expr-entry.
(defun compute-sweet-redirect-readtable ()
  (setq *sweet-readtable*
    ; Create a new readtable from the standard one. We do this because
    ; other characters may have been defined as dispatching macro chars, and 
    ; we need to make sure that they *stop* dispatching.
    ; Starting from the standard readtable gives us a known starting point.
    (let ((new (copy-readtable nil)))
      ; Copy the readtable-case setting so we will continue to use it.
      (setf (readtable-case new) (readtable-case *readtable*))
      (set-syntax-from-char #\# #\' new) ; force # to not be dispatching char.
      (loop for ci from 0 upto *my-char-code-limit*
         do (set-macro-character (code-char ci) #'t-expr-entry nil new))
      new)))

(defun enable-sweet-real ()
  ;(in-readtable sweet-readtable)
  
  (when (setup-enable 'sweet)
    (enable-neoteric-real)
    ; Now create the underlying sweet readtable by tweaking neoteric readtable.
    ; This underlying table is called to read specific expressions.
    (setq *readtable* (copy-readtable *readtable*))
    ; Handle #|...|# and #; specially:
    (set-dispatch-macro-character #\# #\| #'wrap-comment-block)
    (set-dispatch-macro-character #\# #\; #'wrap-comment-datum)
    ; Re-implement backquote and comma, so indentation can happen inside them;
    ; Notice that (read stream t nil t) is replaced with (my-read-datum stream):
    (set-macro-character #\`
      #'(lambda (stream char)
          (declare (ignore char))
          (list 'backquote (my-read-datum stream))))
    (set-macro-character #\,
      #'(lambda (stream char)
          (declare (ignore char))
            (case (my-peek-char stream)
              (#\@ (my-read-char stream)
                   (list *comma-atsign* (my-read-datum stream)))
              (#\. (read-char stream t nil t)
                   (list *comma-dot* (my-read-datum stream)))
              (otherwise (list *comma* (my-read-datum stream))))))
    (setq *underlying-sweet-readtable* *readtable*)

    ; Now create the redirecting readtable.  The idea is that ANY input
    ; will be redirected (through this table) eventually to t-expr and it-expr,
    ; which process the indentation, and they'll call other procedures that
    ; in turn will invoke *underlying-sweet-readtable*.
    (compute-sweet-redirect-readtable)
    (setq *readtable* *sweet-readtable*)) ;|#
  (values))

(defun sweet-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-sweet-real)
    (read stream)))

;;; enablers.cl
;;; Implements sweet-expressions from the "readable" approach for Lisp.
; These macros enable various notations.
; These are macros so we can force compile-time modification
; of the readtable (using eval-when).
; The macros invoke functions with name + "-real" which do the real work.

(defmacro enable-basic-curly ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-basic-curly-real)))

(defmacro enable-full-curly-infix ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-full-curly-infix-real)))

; Synonym.
(defmacro enable-curly-infix ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-full-curly-infix-real)))

(defmacro enable-neoteric ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-neoteric-real)))

(defmacro enable-sweet ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-sweet-real)))

