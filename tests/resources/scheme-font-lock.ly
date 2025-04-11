\version "2.24.0"
%% <- font-lock-keyword-face
%%       ^ font-lock-string-face

#(define* (foo #:optional (bar #f))
;; <- default
;;^ font-lock-keyword-face
;;        ^ default
;;         ^ font-lock-function-name-face
;;             ^ font-lock-builtin-face
;;                        ^ default
;;                             ^ font-lock-constant-face
;;                               ^ default
   (let lp ((j 0))))
%% ^ default
%%  ^ font-lock-keyword-face
%%      ^ font-lock-function-name-face
%%         ^ default
%%             ^ font-lock-number-face
%%              ^ default

#(define foo (lambda (x) x))
%% <- default
%%^ font-lock-keyword-face
%%       ^ font-lock-function-name-face
%%           ^ default
%%            ^ font-lock-keyword-face
%%                   ^ default
%%                       ^ default




#(define-class <my-class> (<parent-class>))
%% <- default
%%^ font-lock-keyword-face
%%             ^ font-lock-type-face
%%                        ^ default
%%                         ^ font-lock-type-face
%%                                       ^ default

#(define-module my-module)
%% <- default
%%^ font-lock-keyword-face
%%              ^ font-lock-type-face
%%                       ^ default

#(define baz "@var{baz}")
%% <- default
%%^ font-lock-keyword-face
%%       ^ font-lock-variable-name-face
%%           ^ font-lock-string-face
%%            ^ (font-lock-escape-face font-lock-doc-markup-face)
%%             ^ font-lock-doc-markup-face
%%                ^ (font-lock-escape-face font-lock-string-face)
%%                 ^ font-lock-string-face
%%                    ^ (font-lock-escape-face font-lock-string-face)
%%                     ^ font-lock-string-face
%%                      ^ default

#(define-syntax-rule (foo () '()))
%% <- default
%%^ font-lock-keyword-face
%%                   ^ default
%%                    ^ font-lock-variable-name-face
%%                        ^ default
%%                           ^ default

#(define-music-function (x) (ly:music?) x)
%% <- default
%%^ font-lock-keyword-face
%%                      ^ default
%%                          ^ default
%%                                      ^ default

char = ##\x
%% <- default
%%   ^ default
%%     ^ font-lock-constant-face
