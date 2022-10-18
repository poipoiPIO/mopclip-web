(ql:quickload '(:hunchentoot :spinneret :lass))

(defparameter *server* 
  (make-instance 
    'tbnl:easy-acceptor
    :port 8000
    :document-root (merge-pathnames "static" (uiop/os:getcwd))
    :address "127.0.0.1"))

(defun start ()
  (tbnl:start *server*))

;; --------------------- Views ---------------------

;;_______ Static dir path methods and class definition _______ 

(defclass static-dir-path ()
  ((sys-path :initarg :path :accessor path)
   (web-route :initarg :route :accessor route)))

(defparameter *static-dirs-path* (list 
  (make-instance 'static-dir-path 
                 :route "/home/lappely/Templates/common-lisp/mopclip-web/web/static/img/"
                 :path "/img/")))

(defgeneric submit-static-dirs-path (dir-path)
  (:documentation "Submit a new path to dispatch table of server"))

(defmethod submit-static-dirs-path ((dir-path static-dir-path))
  (push (tbnl:create-folder-dispatcher-and-handler
          (path dir-path) (route dir-path))
        tbnl:*dispatch-table*))

(defun submit-all-dirs (list-of-static-dirs-path)
  (mapcar #'submit-static-dirs-path list-of-static-dirs-path))

;;_______ Web-pages methods and class definition _______ 

(defclass web-page ()
  ((name :accessor name
         :initarg :name)
   (html :accessor html
         :initarg :html)))

(defparameter *web-pages* (make-hash-table))

(defgeneric compile-webpage (page)
  (:documentation "Returns the markup of selected-page"))

(defmethod compile-webpage ((page web-page)) (html page))

;; --------------------- Views ---------------------

(defparameter *css* 
  '((body
    :background-color "alice-blue")))

(defparameter *code-examples*
  (list 
(cons 'csv (quote 
"open Mopclip;

val cell = (manyC anyCharP) mapP implode;
val line = cell sepBy (charP #\",\");
val csv = line sepBy (charP #\"\n\");"))
(cons 'array-parser (quote 
"val digitArrayP = 
    ((charP #\"[\")
      *> (manyC (digitP <* (charP #\",\")))
      <* (charP #\"]\"))
      mapP (map (fn c => (ord c) - 48));

runParser digitArrayP \"[1,2,3,]\";
- val it = Success ([1,2,3],\"\")"))))

(setf (gethash 'main-page *web-pages*)
  (make-instance 'web-page :name "main"
    :html (spinneret:with-html-string
      (:html 
        (:head 
          (:style (apply #'lass:compile-and-write *css*)))
        (:body
          (:header 
            (:div.logo 
              (:h1 (:a :attrs (list :href "https://github.com/poipoiPIO/mopclip")
                       "mopclip"))))
          (:main
            (:div.preview 
              (:h2 "Mopclip :: human-friendly monadic parser-combinator library")
              (:img :attrs (list :src "img/logo.svg" :alt "Mopclip logo"))
              (:p "Mopclip is disigned to be an ultimative solution at the parser
                  creation task field.")
              (:p "With Mopclip you can easilly write your own syntactic and
                  lexical analyzers using the human-friendly DSL and get the cool
                  and fast parser in less than blink of an eye!")
              (:p (:a.button :attrs (list :href "https://github.com/poipoiPIO/mopclip") 
                             "Goto Github page!")))
            (:div.code-examples 
              (:h2 "I can't wait, show me the code!")
              (:p "Mopclip it's the lightweight implementation of
                  Parser-combinators library for SML programming
                  language. Let's take a look at some examples of 
                  applications using Mopclip:")
              (:div.code-example 
                (:pre (cdr (assoc 'csv *code-examples*)))
                (:h3 "Simple csv-like language parser")
                (:p "As you can see, we using some basic combinators 
                    (like a manyC or sepBy) with a couple of basic parsers 
                    (charP, anyCharP), to build rather more complex parsers 
                    (line parser, etc.)"))
              (:div.code-example 
                (:pre (cdr (assoc 'array-parser *code-examples*)))
                (:h3 "Simple ruby-like array parser")
                (:p "A kind of good example of using right and
                    left applicative operations."))))
            (:footer
              (:span "Site created by Lily Appely. 2022")))))))

;; --------------------- Routing ---------------------
(tbnl:define-easy-handler (main :uri "/") ()
  (compile-webpage (gethash 'main-page *web-pages*)))

;; --------------------- Run-serve ---------------------
(submit-all-dirs *static-dirs-path*)
(start)
