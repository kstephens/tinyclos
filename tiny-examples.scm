; Mode: Scheme
;
;
; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
; Some simple examples of using Tiny CLOS and its MOP.
; 
; Much of this stuff corresponds to stuff in AMOP (The Art of the
; Metaobject Protocol).
;

;***
;
; This is a useful sort of helper function.  Note how it uses the
; introspective part of the MOP.  The first few pages of chapter
; two of the AMOP discuss this.
;
; Note that this introspective MOP doesn't support back-links from
; the classes to methods and generic functions.  Is that worth adding?
;
;
(define initialize-slots
    (lambda (object initargs)
      (let ((not-there (list 'shes-not-there)))
	(for-each (lambda (slot)
		    (let ((name (car slot)))
		      (let ((value  (getl initargs name not-there)))
			(if (eq? value not-there)
			    'do-nothing
			    (slot-set! object name value)))))
		  (class-slots (class-of object))))))



;***
;
; A simple class, just an instance of <class>.  Note that we are using
; make and <class> rather than make-class to make it.  See Section 2.4
; of AMOP for more on this.
;
;

(define <pos> (make <class>                          ;[make-class 
		    'direct-supers (list <object>)   ;  (list <object>) 
		    'direct-slots  (list 'x 'y)))    ;  (list 'x 'y)]

(add-method initialize
    (make-method (list <pos>)
      (lambda (call-next-method pos initargs)
	(call-next-method)
	(initialize-slots pos initargs))))

(define p1 (make <pos> 'x 1 'y 2))
(define p2 (make <pos> 'x 3 'y 5))


;***
;
; Another way of writing that class definition, that achives better
; `encapsulation' by using slot names that are unique keys, rather
; than symbols.
;
;

(define <pos>)
(define pos-x (make-generic))
(define pos-y (make-generic))
(define move  (make-generic))

(let ((x (vector 'x))
      (y (vector 'y)))
  
  (set! <pos> (make <class>
		    'direct-supers (list <object>)
		    'direct-slots  (list x y)))

  (add-method pos-x
      (make-method (list <pos>)
	(lambda (call-next-method pos) (slot-ref pos x))))
  (add-method pos-y
      (make-method (list <pos>)
	(lambda (call-next-method pos) (slot-ref pos y))))

  (add-method move
      (make-method (list <pos>)
        (lambda (call-next-method pos new-x new-y)
	  (slot-set! pos x new-x)
	  (slot-set! pos y new-y))))

  (add-method initialize
      (make-method (list <pos>)
	(lambda (call-next-method pos initargs)
	  (move pos (getl initargs 'x 0) (getl initargs 'y 0)))))
  )


(define p3 (make <pos> 'x 1 'y 2))
(define p4 (make <pos> 'x 3 'y 5))


;***
;
; Class allocated slots.
;
; In Scheme, this extension isn't worth a whole lot, but what the hell.
;
;

(define <class-slots-class>
    (make-class (list <class>)
		(list)))

(add-method compute-getter-and-setter
    (make-method (list <class-slots-class>)
      (lambda (call-next-method class slot allocator)
	(if (null? (memq ':class-allocation slot))
	    (call-next-method)
	    (let ((cell '()))
	      (list (lambda (o) cell)
		    (lambda (o new) (set! cell new) new)))))))


;
; Here's a silly program that uses class allocated slots.
;
;
(define <ship>
    (make <class-slots-class>
	  'direct-supers (list <object>)
	  'direct-slots  (list 'name
			       '(all-ships :class-allocation))))

(add-method initialize
    (make-method (list <ship>)
      (lambda (call-next-method ship initargs)
	(call-next-method)
	(initialize-slots ship initargs)
	(slot-set! ship
		   'all-ships
		   (cons ship (slot-ref ship 'all-ships))))))

(define siblings (make-generic))
(add-method siblings
    (make-method (list <ship>)
      (lambda (call-next-method ship)
	(remove ship (slot-ref ship 'all-ships)))))

(define s1 (make <ship> 'name 's1))
(define s2 (make <ship> 'name 's2))
(define s3 (make <ship> 'name 's3))



;***
;
; Here's a class of class that allocates some slots dynamically.
;
; It has a layered protocol (dynamic-slot?) that decides whether a given
; slot should be dynamically allocated.  This makes it easy to define a
; subclass that allocates all its slots dynamically.
;
;
(define <dynamic-class>
    (make-class (list <class>)
		(list 'alist-g-n-s)))


(define dynamic-slot? (make-generic))

(add-method dynamic-slot?
    (make-method (list <dynamic-class>)
      (lambda (call-next-method class slot)
	(memq :dynamic-allocation (cdr slot)))))



(define alist-getter-and-setter
    (lambda (dynamic-class allocator)
      (let ((old (slot-ref dynamic-class 'alist-g-n-s)))
	(if (null? old)
	    (let ((new (allocator (lambda () '()))))
	      (slot-set! dynamic-class 'alist-g-n-s new)
	      new)
	    old))))


(add-method compute-getter-and-setter
    (make-method (list <dynamic-class>)
      (lambda (call-next-method class slot allocator)
	(if (null? (dynamic-slot? class slot))
	    (call-next-method)
	    (let* ((name (car slot))
		   (g-n-s (alist-getter-and-setter class allocator))
		   (alist-getter (car g-n-s))
		   (alist-setter (cadr g-n-s)))
	      (list (lambda (o)
		      (let ((entry (assq name  (alist-getter o))))
			(if (null? entry)
			    '()
			    (cdr entry))))
		    (lambda (o new)
		      (let* ((alist (alist-getter o))
			     (entry (assq name alist)))
			(if (null? entry)
			    (alist-setter o
			                  (cons (cons name new) alist))
			    (set-cdr! entry new))
			new))))))))


(define <all-dynamic-class>
    (make-class (list <dynamic-class>)
		(list)))

(add-method dynamic-slot?
    (make-method (list <all-dynamic-class>)
      (lambda (call-next-method class slot) #t)))
	    


;
; A silly program that uses this.
;
;
(define <person> (make <all-dynamic-class>
		       'direct-supers (list <object>)
		       'direct-slots  (list 'name 'age 'address)))

(add-method initialize
    (make-method (list <person>)
      (lambda (call-next-method person initargs)
	(initialize-slots person initargs))))


(define person1 (make <person> 'name 'sally))
(define person2 (make <person> 'name 'betty))
(define person3 (make <person> 'name 'sue))


;***
;
; A ``database'' class that stores slots externally.
;
;

(define <db-class>
  (make-class (list <class>)
	      (list 'id-g-n-s)))

(define id-getter-and-setter
    (lambda (db-class allocator)
      (let ((old (slot-ref db-class 'id-g-n-s)))
	(if (null? old)
	    (let ((new (allocator db-allocate-id)))
	      (slot-set! class 'id-g-n-s new)
	      new)
	    old))))

(add-method compute-getter-and-setter
    (make-method (list <db-class>)
      (lambda (call-next-method class slot allocator)
	(let* ((id-g-n-s (id-getter-and-setter class allocator))
	       (id-getter (car id-g-n-s))
	       (id-setter (cadr id-g-n-s))
	       (slot-name (car slot)))
	  (list (lambda (o)
		  (db-lookup (id-getter o) slot-name)) 
		(lambda (o new)
		  (db-store  (id-getter o) slot-name new)))))))


;***
;
; A kind of generic that supports around methods.
;
;
(define make-around-generic
    (lambda () (make <around-generic>)))

(define make-around-method
    (lambda (specializers procedure)
      (make <around-method>
	    'specializers specializers
	    'procedure procedure)))


(define <around-generic> (make <entity-class>
			       'direct-supers (list <generic>)))
(define <around-method>  (make <class>
			       'direct-supers (list <method>)))


(define around-method?   (make-generic))

(add-method around-method?
    (make-method (list <method>)
      (lambda (call-next-method x) #f)))
(add-method around-method?
    (make-method (list <around-method>)
      (lambda (call-next-method x) #t)))


(add-method compute-methods
    (make-method (list <around-generic>)
      (lambda (call-next-method generic)
	(let ((normal-compute-methods (call-next-method)))
	  (lambda (args)
	    (let ((normal-methods (normal-compute-methods args)))
	      (append
	        (filter-in around-method?
			   normal-methods)
		(filter-in (lambda (m) (not (around-method? m)))
			   normal-methods))))))))


;
; And a simple example of using it.
;
;
(define <baz> (make-class (list <object>) (list)))
(define <bar> (make-class (list <baz>)    (list)))
(define <foo> (make-class (list <bar>)    (list)))


(define test-around
    (lambda (generic)
      (add-method generic
	  (make-method        (list <foo>)
	                      (lambda (cnm x) (cons 'foo (cnm)))))

      (add-method generic
	  (make-around-method (list <bar>)
			      (lambda (cnm x) (cons 'bar (cnm)))))

      (add-method generic
	  (make-method        (list <baz>)
	                      (lambda (cnm x) '(baz))))

      (generic (make <foo>))))


(equal? (test-around (make-generic))        '(foo bar baz))
(equal? (test-around (make-around-generic)) '(bar foo baz))
