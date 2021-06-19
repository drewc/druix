(define-module (druix utils plists)
  #:use-module (ice-9 match)
  #:export (plist-get
            plist-fold
            plist-add
            plist-delete
            plist-put
            plist-new))

(define* (plist-get plist property #:optional (default #f))
  "Return a value of PROPERTY from PLIST.
Return the #:optional (default #f) if PROPERTY does not exist."
  (match plist
    ((prop val rest ...)
     (if (eq? prop property)
         val
         (plist-get rest property default)))
    (_ default)))

(define (plist-fold proc init plist)
  "Fold over property/value elements of PLIST.
Call (PROC PROPERTY VALUE RESULT) for each property, using INIT as the
initial value of RESULT."
  (let loop ((result init)
             (current plist))
    (match current
      (()
       result)
      ((prop val rest ...)
       (loop (proc prop val result)
             rest)))))



(define (plist-add plist property value)
  "Add PROPERTY/VALUE pair to PLIST."
  (cons* property value plist))

(define (plist-delete plist property)
  "Remove all PROPERTY elements from PLIST."
  (plist-fold (lambda (prop val res)
                (if (eq? prop property)
                    res
                    (plist-add res prop val)))
              '()
              plist))

(define (plist-put plist property value)
  "Return new plist by changing or adding PROPERTY/VALUE pair in PLIST."
  (plist-add (plist-delete plist property)
             property value))

(define (plist-new old-plist . add-plist)
  "Return new plist by adding property/value pairs from ADD-PLIST to
OLD-PLIST."
  (plist-fold (lambda (prop val res)
                (plist-put res prop val))
              old-plist
              add-plist))
