
(load "trivial-sockets")
(load "trivial-http")
(require :closure-html)
(require :cxml)
(require :drakma)

;(thttp:http-get "http://www.trademe.co.nz/Browse/Motors/Cars.aspx")
;/Browse/Motors/CarsMoreMakes.aspx

;; modified from http://clisp.cons.org/impnotes/socket.html
(defun wget-text (uri file)
  (destructuring-bind (status headers stream) (thttp:http-get uri)
    (if (= status 200)
	;; dump the data into the output file
	(with-open-file (out file :direction :output)
	  (loop :for line = (read-line stream nil nil) :while line
	     :do (write-line line out))
	  (close stream)))
    (list status headers)))


;(wget-text "http://www.trademe.co.nz/Browse/Motors/Cars.aspx" "Cars.html")
;(wget-text "http://www.trademe.co.nz/Browse/Motors/CarsMoreMakes.aspx" "moremakes.html")

;(defvar *cars* nil)
;(setf *cars* (chtml:parse #p"Cars.html" (chtml:make-lhtml-builder)))
;(setf *cars* (chtml:parse #p"Cars.html" (cxml-stp:make-builder)))
;(setf *cars* (chtml:parse #p"makes.html" (chxml-stp:make-lhtml-builder)))

;; Cars : search type query
;        (:SELECT ((:ID "searchType") (:NAME "searchType"))
;         (:OPTION ((:VALUE "all")) "All items")
;         (:OPTION ((:VALUE "0001-")) "Cars, bikes & boats")
;         (:OPTION ((:VALUE "0001-0268-") (:SELECTED "selected")) " Used Cars")

;; Cars : makes query
;          (:TABLE ((:ID "makes"))
            ;; (:TBODY NIL
            ;;  (:TR NIL
            ;;   (:TD ((:CLASS "make_item_col0"))
            ;;    (:A
            ;;     ((:HREF
            ;;       "/Trade-Me-Motors/Cars/Alfa-romeo/mcat-0001-0268-0269-.htm"))
            ;;     "Alfa romeo")
            ;;    (:SPAN ((:CLASS "count")) "(168)"))

(defun test-makes ()
  (let* ((doc (chtml:parse #p"moremakes.html" (cxml-stp:make-builder)))
	 (tab (find-car-make-table doc))
	 (cells (find-cells-in-table tab)))
					;(cells (find-cells-in-table tab "make_item_col")))
    cells));(get-cars-from-cells cells)))
       
(defun find-car-make-table (doc &optional (table-id "makeTable"))
  (stp:find-recursively-if 
   #'(lambda (tab) 
       (and (typep tab 'stp:element)
	    (equal (stp:local-name tab) "table")
	    (equal (stp:attribute-value tab "id") table-id)))
   doc))

(defun find-cells-in-table (table &optional col-prefix)
  "return a list of html <td> cells in table"
  (stp:filter-recursively 
   #'(lambda (cell) 
       (and (typep cell 'stp:element)
	    (equal (stp:local-name cell) "td")
	    (if col-prefix 
		(string-equal (stp:attribute-value cell "class") col-prefix :end1 (length col-prefix)) 
		t)))
   table))
;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defun html-a-p (elt)
  "elt is an html <a> tag"
  (and (typep elt 'stp:element)
       (equal (stp:local-name elt) "a")))

(defun html-span-class-count (elt)
  "elt is an html <span class=count>"
  (and (typep elt 'stp:element)
       (equal (stp:local-name elt) "span")
       (equal (stp:attribute-value elt "class") "count")))

(defun parse-span-count (elt)
  (let ((s (stp:string-value elt)))
    (parse-integer s :start 1 :end (1- (length s)))))

(defun parse-href (elt)
  (stp:attribute-value elt "href"))

(defun find-and-transform-items (cells &key test (transform #'stp:string-value))
  "filter html for items matching test, transform the results returning a flattened list"
  (reduce #'append 
	  (mapcar #'(lambda (cell)
		      (mapcar transform (stp:filter-recursively test cell)))
		  cells)))

(defun car-make-names (cells)
  (find-and-transform-items cells :test #'html-a-p ))

(defun car-make-urls (cells)
  (find-and-transform-items cells :test #'html-a-p :transform #'parse-href))

(defun car-make-counts (cells)
  (find-and-transform-items :test #'html-span-class-count :transform #'parse-span-count))

(defun parse-make-data ()
  (let ((makes (test-makes)))
    (mapcar #'list  (car-make-names makes) (car-make-urls makes) (car-make-counts makes))))





;;;;;;;;;;;;;
(defun get-cars-from-cells (cells)
  "get car info from href and span, one per cell"
  (mapcar #'(lambda (cell)
	      (let* ((car-links (stp:filter-recursively 
				 #'(lambda (e) 
				    (and (typep e 'stp:element)
					 (equal (stp:local-name e) "a")))
				cell))
		     (car-makes (mapcar #'stp:string-value car-links))
		     (car-urls (mapcar #'stp:string-value (stp:find-attribute-named car-links "href")))
		     (car-count-elts (stp:filter-recursively 
				      #'(lambda (e) 
					  (and (typep e 'stp:element)
					       (equal (stp:local-name e) "span")
					       (equal (stp:attribute-value e "class") "count")))
				     cell))
		     (car-counts (mapcar #'(lambda (e) 
					     (if e 
						 (string-trim "()" (stp:string-value e))))
					 car-count-elts)))
		(list car-makes car-urls car-counts)))
	  cells))

(defun get-cars-from-br-list (cells)
  "get car info from href and span, a <br> delimited list per cell"
  (mapcar #'(lambda (cell)
	      (let* ((car-link (stp:find-recursively-if 
				#'(lambda (e) 
				    (and (typep e 'stp:element)
					 (equal (stp:local-name e) "a")))
				cell))
		     (car-make (stp:string-value car-link))
		     (car-url (stp:string-value (stp:find-attribute-named car-link "href")))
		     (car-count-elt (stp:find-recursively-if 
				     #'(lambda (e) 
					 (and (typep e 'stp:element)
					      (equal (stp:local-name e) "span")
					      (equal (stp:attribute-value e "class") "count")))
				     cell))
		     (car-count (if car-count-elt 
				    (string-trim "()" (stp:string-value car-count-elt)))))
		(list car-make car-url car-count)))
	  cells))



  
(defun show-google-hits (term)
    (let* ((query (list (cons "q" term)))
	   (str (drakma:http-request "http://www.google.com/search"
				     :parameters query))
	   (document (chtml:parse str (cxml-stp:make-builder))))
      (stp:do-recursively (a document)
	(when (and (typep a 'stp:element)
		   (equal (stp:local-name a) "a")
		   (equal (stp:attribute-value a "class") "l"))
	  (format t "~A:~%  ~A~%~%"
		  (stp:string-value a)
		  (stp:attribute-value a "href"))))))

					;<a onmousedown="return rwt(this,'','','res','1','AFQjCNFoOPUY7Qgs4J_NdQtbcQ3DnPk2Uw','&amp;sig2=NIXbL2ATGRekL84HzNM6_g')" class="l" href="http://en.wikipedia.org/wiki/Lisp_(programming_language)"><em>Lisp</em> (programming language) - Wikipedia, the free encyclopedia</a>

(defun show-google-stp (term)
  (let* ((query (list (cons "q" term)))
	 (str (drakma:http-request "http://www.google.com/search"
				   :parameters query)))
    (chtml:parse str (cxml-stp:make-builder))))
