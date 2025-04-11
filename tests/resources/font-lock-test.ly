\version "2.24.0"
%% <- font-lock-keyword-face
%%       ^ font-lock-string-face
\language "english"
%% <- font-lock-keyword-face
%%        ^ font-lock-string-face

music = \relative {
%% <- default
%%    ^ default
%%      ^ font-lock-keyword-face
%%                ^ default
  \clef bass
%%^ lilypond-ts-font-lock-identifier-face
%%      ^ font-lock-constant-face
  \time 4/4
%%^ font-lock-keyword-face
%%      ^ font-lock-number-face
  \key a \minor
%%^ lilypond-ts-font-lock-identifier-face
%%     ^ default
%%       ^ lilypond-ts-font-lock-identifier-face
  \tempo "Allegro" 4=90
%%^ font-lock-keyword-face
%%       ^ font-lock-string-face
%%                 ^ lilypond-ts-font-lock-duration-face
%%                  ^ default
%%                   ^ lilypond-ts-font-lock-duration-face
  c'4\mp\< d-! e\0 f_> |
%%^ default
%%  ^ lilypond-ts-font-lock-duration-face
%%   ^ lilypond-ts-font-lock-event-face
%%      ^ lilypond-ts-font-lock-articulation-face
%%         ^ default
%%          ^ lilypond-ts-font-lock-articulation-face
%%             ^ default
%%              ^ font-lock-number-face
%%                 ^ default
%%                  ^ lilypond-ts-font-lock-articulation-face
%%                     ^ default
  e\(\> d^> c b\)\! |
%%^ default
%% ^ lilypond-ts-font-lock-phrasing-slur-face
%%   ^ lilypond-ts-font-lock-articulation-face
%%      ^ default
%%       ^ lilypond-ts-font-lock-articulation-face
%%          ^ default
%%            ^ default
%%             ^ lilypond-ts-font-lock-phrasing-slur-face
%%               ^ lilypond-ts-font-lock-articulation-face
%%                  ^ default
  \once\override Voice.NoteHead.stencil = ##f
%%^ font-lock-keyword-face
%%               ^ lilypond-ts-font-lock-context-face
%%                    ^ default
%%                     ^ font-lock-type-face
%%                             ^ default
%%                                      ^ default
%%                                        ^ font-lock-constant-face
  a4[\startTextSpan 4\^ 4~\stopTextSpan 4:32 |
%%^ default
%% ^ lilypond-ts-font-lock-duration-face
%%  ^ lilypond-ts-font-lock-beam-face
%%   ^ lilypond-ts-font-lock-event-face
%%                  ^ lilypond-ts-font-lock-duration-face
%%                   ^ lilypond-ts-font-lock-event-face
%%                      ^ lilypond-ts-font-lock-duration-face
%%                       ^ lilypond-ts-font-lock-tie-face
%%                        ^ lilypond-ts-font-lock-event-face
%%                                      ^ lilypond-ts-font-lock-duration-face
%%                                       ^ lilypond-ts-font-lock-event-face
%%                                           ^ default
  <c e>2^"div." <c' e,>^\markup\italic { unisono }
%%^ default
%%     ^ lilypond-ts-font-lock-duration-face
%%      ^ default
%%       ^ font-lock-string-face
%%              ^ default
%%                      ^ lilypond-ts-font-lock-markup-keyword-face
%%                             ^ lilypond-ts-font-lock-markup-face
%%                                     ^ default
  d8(\=2 c[) b a?] gs!2\trill |
%%^ default
%% ^ lilypond-ts-font-lock-duration-face
%%  ^ lilypond-ts-font-lock-slur-face
%%   ^ font-lock-keyword-face
%%     ^ font-lock-number-face
%%       ^ default
%%        ^ lilypond-ts-font-lock-beam-face
%%         ^ lilypond-ts-font-lock-slur-face
%%           ^ default
%%             ^ default
%%               ^ lilypond-ts-font-lock-beam-face
%%                 ^ default
%%                    ^ lilypond-ts-font-lock-duration-face
%%                     ^ lilypond-ts-font-lock-event-face
%%                            ^ default
  a\breve*1/2
%%^ default
%% ^ lilypond-ts-font-lock-duration-face
}
%% <- default

\score {
%% <- font-lock-keyword-face
%%     ^ default
  <<
%%^ default
    \new Staff \with {
%%  ^ font-lock-keyword-face
%%       ^ lilypond-ts-font-lock-context-face
%%             ^ font-lock-keyword-face
%%                   ^ default
      instrumentName = "I"
%%    ^ default
%%                   ^ default
%%                     ^ font-lock-string-face
      \consists Horizontal_bracket_engraver
%%    ^ font-lock-keyword-face
%%              ^ default
    } \music
%%  ^ default
%%    ^ lilypond-ts-font-lock-identifier-face
  >>
%%^ default
  \layout {
%%^ font-lock-keyword-face
%%        ^ default
    \context {
%%  ^ font-lock-keyword-face
%%           ^ default
      \Score
%%    ^ lilypond-ts-font-lock-context-face
      %% This is a comment
%%    ^ font-lock-comment-face
      \applyContext #(lambda (ctx)
;;    ^ font-lock-keyword-face
;;                  ^ default
;;                    ^ font-lock-keyword-face
;;                           ^ default
                       % wrong comment syntax
;;                     ^ font-lock-warning-face
                       (message "~a\n bar" (ly:context-children ctx)))
%%                     ^ default
%%                              ^ font-lock-string-face
%%                                 ^ (font-lock-escape-face font-lock-string-face)
%%                                   ^ font-lock-string-face
%%                                         ^ default
%%                                                              ^ default
    }
%%  ^ default
  }
%%^ default
}
%% <- default
