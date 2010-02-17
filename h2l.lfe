(defmodule h2l
  (export (pipe 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Program to convert nitrogen hrl file to lfe.
;;
;;   usage: cat wf.inc | erl -noshell -s h2l pipe 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pipe() (pipe '()))

(defun pipe(res) 
  (case (: io get_chars '"" 8192) 
    ((tuple 'eof) 
     (process res))
    (text 
     (pipe (cons res text)))))

(defun process(data)
  (let ((name (generate_filename)))
    (save name data)
    (let (((tuple 'ok ast) (: epp parse_file name '() '())))
      (print ast)
      (: file delete name)
      (: init stop))))

(defun generate_filename() 
  (let (((tuple a b c) (now))
        (n (node())))
    (++ '"h2l" (: lists flatten (: io_lib format '"~p~p~p~p" (list n a b c))))))

(defun save(name data)
  (let (((tuple 'ok f) (: file open name (list 'read 'write))))
    (: io write f '"~s~n" (list data))
    (: file close f)))

(defun print (ast)
  (lc ((<- c ast))
    (lfe c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ast to lfe 
;;

(defun lfe 
  (((tuple 'attribute _line 'file _recordname))
   (: io format '";; Generated with git://github.com/cadar/hrl-to-lfe.git~n~n"))
  
  (((tuple 'attribute _ 'record recorddata))
   (let (((tuple name recs) recorddata))
     (: io format '"(defrecord ~p" (list name))
     (lc ((<- c recs))
       (rec c))
     (: io format '")~n")))

  (((tuple 'eof _))
   (let (((tuple (tuple y m d) (tuple h mm s)) (: erlang universaltime)))
     (: io format '"~n;; done -")
     (: io format '" ~B-~2.10.0B-~2.10.0B" (list y m d))
     (: io format '" ~2.10.0B:~2.10.0B:~2.10.0B~n" (list h mm s))))

  ((all) 
   (: error_logger error_msg '"Untransformed record: ~p~n" (list all))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  record type 
;;
(defun rec 
  (((tuple 'record_field _ atomdata))
   (first_field atomdata))

  (((tuple 'record_field _ atomdata1 atomdata2))
   (: io format '" (")
   (first_field atomdata1)
   (field atomdata2)
   (: io format '")"))
  
  (((tuple 'record_field _ atomdata1 atomdata2 atomdata3))
   (: io format '" (")
   (first_field atomdata1)
   (first_field atomdata2)
   (first_field atomdata3)
   (: io format '")"))
  
  ((all) 
   (: error_logger error_msg '"Untransformed rec option: ~p~n" (list all))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;%
;;
;;  print field type
;;

(defun first_field 
  (((tuple 'atom _ fieldname)) 
   (: io format '" ~p" (list fieldname))))

(defun field
  (((tuple 'atom _ fieldname)) (: io format '" '~p" (list fieldname)))
  (((tuple 'string _ string)) (: io format '" '\"~s\"" (list string)))
  (((tuple 'integer _ nr)) (: io format '" ~p" (list nr)))
  (((tuple 'cons _ first _rest)) (field first))
  (((tuple 'nil _ )) (: io format '" '()"))
  (((tuple 'tuple _ li))
   (: io format '" '()")
   (first_field (hd li))
   (lc ((<- c (tl li)))
     (field c))
   (: io format '")"))

  ((all) 
   (: error_logger error_msg '"Untransformed field: ~p~n" (list all))))

 
