\version "2.24.0"
%% <- font-lock-keyword-face
%%       ^ font-lock-string-face
\language "english"
%% <- font-lock-keyword-face
%%        ^ font-lock-string-face

music = \relative {
%% <- nil
%%      ^ font-lock-keyword-face
%%               ^ nil
  \time 4/4
%%^ font-lock-keyword-face
%%      ^ font-lock-number-face
  \key a \minor
%%^ font-lock-variable-use-face
%%    ^ nil
%%       ^ font-lock-variable-use-face
  \tempo "Allegro" 4=90
%%^ font-lock-keyword-face
%%       ^ font-lock-string-face
%%                 ^ bold
%%                  ^ nil
%%                   ^ bold
  c'4\mp\< d-! e\0 f_> |
%% <- nil
%%  ^ bold
%%   ^ font-lock-builtin-face
%%      ^ (bold font-lock-builtin-face)
%%        ^ nil
%%          ^ (bold font-lock-builtin-face)
%%           ^ (bold font-lock-builtin-face)
%%            ^ nil
%%              ^ font-lock-number-face
%%                ^ nil
%%                  ^ (bold font-lock-builtin-face)
%%                   ^ (bold font-lock-builtin-face)
%%                    ^ nil
  e\(\> d^> c b\)\! |
%% <- nil
%% ^ (bold font-lock-variable-name-face)
%%   ^ (bold font-lock-builtin-face)
%%     ^ nil
%%       ^ (bold font-lock-builtin-face)
%%        ^ (bold font-lock-builtin-face)
%%         ^ nil
%%             ^ (bold font-lock-variable-name-face)
%%               ^ (bold font-lock-builtin-face)
%%                 ^ nil
  \once\override Voice.NoteHead.stencil = ##f
%%^ font-lock-keyword-face
%%               ^ (bold font-lock-type-face)
%%                    ^ nil
%%                     ^ font-lock-type-face
%%                             ^ nil
%%                                        ^ font-lock-constant-face
  a4[\startTextSpan 4\^ 4~\stopTextSpan 4:32 |
%% <- nil
%% ^ bold
%%  ^ nil
%%   ^ font-lock-builtin-face
%%                  ^ bold
%%                   ^ font-lock-builtin-face
%%                      ^ bold
%%                       ^ nil
%%                        ^ font-lock-builtin-face
%%                                      ^ bold
%%                                       ^ font-lock-builtin-face
%%                                          ^ nil
  <c e>2^"div." <c' e,>^\markup\italic { unisono }
%% <- nil
%%     ^ bold
%%      ^ nil
%%       ^ font-lock-string-face
%%             ^ nil
%%                      ^ (bold font-lock-function-call-face)
%%                             ^ font-lock-function-call-face
%%                                    ^ nil
  d8(\=2 c[) b a?] gs!2\trill |
%% <- nil
%% ^ bold
%%  ^ nil
%%   ^ font-lock-keyword-face
%%     ^ font-lock-number-face
%%      ^ nil
%%                    ^ bold
%%                     ^ font-lock-builtin-face
%%                           ^ nil
  a\breve*1/2
%% <- nil
%% ^ bold
}
%% <- nil

\score {
%% <- font-lock-keyword-face
%%    ^ nil
  <<
%% <- nil
    \new Staff \with {
%%  ^ font-lock-keyword-face
%%       ^ (bold font-lock-type-face)
%%             ^ font-lock-keyword-face
%%                  ^ nil
      instrumentName = "I"
%% <- nil
%%                     ^ font-lock-string-face
      \consists Horizontal_bracket_engraver
%%    ^ font-lock-keyword-face
%%             ^ nil
    } \music
%% <- nil
%%    ^ font-lock-variable-use-face
  >>
%% <- nil
  \layout {
%%^ font-lock-keyword-face
%%       ^ nil
    \context {
%%  ^ font-lock-keyword-face
%%          ^ nil
      \Score
%%    ^ (bold font-lock-variable-use-face)
      %% This is a comment
%%    ^ font-lock-comment-face
      \applyContext #(lambda (ctx)
;;    ^ font-lock-keyword-face
;;                 ^ nil
;;                    ^ font-lock-keyword-face
;;                          ^ nil
                       (message "~a" (ly:context-children ctx)))
%% <- nil
%%                              ^ font-lock-string-face
%%                                  ^ nil
    }
%% <- nil
  }
%% <- nil
}
%% <- nil
