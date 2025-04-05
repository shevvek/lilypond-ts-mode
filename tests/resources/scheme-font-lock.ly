\version "2.24.0"
%% <- font-lock-keyword-face
%%       ^ font-lock-string-face

#(define* (foo #:optional (bar #f))
;; <- nil
;;^ font-lock-keyword-face
;;       ^ nil
;;         ^ font-lock-function-name-face
;;             ^ font-lock-builtin-face
;;                       ^ nil
;;                             ^ font-lock-constant-face
;;                               ^ nil
   (let lp ((j 0))
;; <- nil
;;  ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face
;;        ^ nil
;;             ^ font-lock-number-face
;;              ^ nil
     (if (< j 10)
;; <- nil
;;    ^ font-lock-keyword-face
;;      ^ nil
;;            ^ font-lock-number-face
;;              ^ nil
         (lp (1+ j) bar))))
%% <- nil

#(define foo (lambda (x) x))
%% <- nil
%%^ font-lock-keyword-face
%%       ^ font-lock-function-name-face
%%          ^ nil
%%            ^ font-lock-keyword-face
%%                  ^ nil

#(define lily-fun
;; <- nil
;;^ font-lock-keyword-face
;;       ^ font-lock-function-name-face
   (define-void-function (arg) (scheme?)
;; <- nil
;;  ^ font-lock-keyword-face
;;                      ^ nil
     (ly:message "Boo! ~a" (*parser*))))
%% <- nil
%%               ^ font-lock-string-face
%%                        ^ nil
%%                          ^ font-lock-variable-use-face
%%                                  ^ nil

#(define var (+ 40 2))
%% <- nil
%%^ font-lock-keyword-face
%%       ^ font-lock-variable-name-face
%%          ^ nil
%%              ^ font-lock-number-face
%%                 ^ font-lock-number-face
%%                  ^ nil

#(define-syntax (do-nothing)
;; <- nil
;;^ font-lock-keyword-face
;;             ^ nil
;;               ^ font-lock-variable-name-face
;;                         ^ nil
   '())
%% <- nil

#(define-class <my-class> (<parent-class>))
%% <- nil
%%^ font-lock-keyword-face
%%             ^ font-lock-type-face
%%                       ^ nil
%%                         ^ font-lock-type-face
%%                                       ^ nil

#(define-module my-module)
%% <- nil
%%^ font-lock-keyword-face
%%              ^ font-lock-type-face
%%                       ^ nil

#(define ((((spicy-curried-defun a) b) c) d)
;; <- nil
;;^ font-lock-keyword-face
;;      ^ nil
;;           ^ font-lock-function-name-face
;;                              ^ nil
   (+ a b c d))
%% <- nil

markup-notehead =
%% <- nil
  #(define-music-function (m) (markup?)
;; <- nil
;;  ^ font-lock-keyword-face
;;                       ^ nil
     "Test nested language blocks."
%%   ^ font-lock-string-face
     #{
%% <- nil
       \override NoteHead.stencil = #(lambda (grob)
;;     ^ font-lock-keyword-face
;;               ^ font-lock-type-face
;;                       ^ nil
;;                                    ^ font-lock-keyword-face
;;                                          ^ nil
                                       (grob-interpret-markup grob #{
%% <- nil
                                         \markup \rotate #90 \m
%%                                       ^ (font-lock-function-call-face)
%%                                               ^ font-lock-function-call-face
%%                                                      ^ nil
%%                                                        ^ font-lock-number-face
%%                                                           ^ font-lock-variable-use-face
                                       #}))
%% <- nil
     #})
%% <- nil

char = ##\x
%% <- nil
%%     ^ font-lock-constant-face
