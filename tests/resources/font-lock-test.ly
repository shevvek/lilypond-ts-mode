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
  \clef bass
%%^ lilypond-ts-font-lock-identifier-face
%%      ^ font-lock-constant-face
  \time 4/4
%%^ font-lock-keyword-face
%%      ^ font-lock-number-face
  \key a \minor
%%^ lilypond-ts-font-lock-identifier-face
%%    ^ nil
%%       ^ lilypond-ts-font-lock-identifier-face
  \tempo "Allegro" 4=90
%%^ font-lock-keyword-face
%%       ^ font-lock-string-face
%%                 ^ lilypond-ts-font-lock-duration-face
%%                  ^ nil
%%                   ^ lilypond-ts-font-lock-duration-face
  c'4\mp\< d-! e\0 f_> |
%% <- nil
%%  ^ lilypond-ts-font-lock-duration-face
%%   ^ lilypond-ts-font-lock-event-face
%%      ^ lilypond-ts-font-lock-articulation-face
%%        ^ nil
%%          ^ lilypond-ts-font-lock-articulation-face
%%            ^ nil
%%              ^ font-lock-number-face
%%                ^ nil
%%                  ^ lilypond-ts-font-lock-articulation-face
%%                    ^ nil
  e\(\> d^> c b\)\! |
%% <- nil
%% ^ lilypond-ts-font-lock-phrasing-slur-face
%%   ^ lilypond-ts-font-lock-articulation-face
%%     ^ nil
%%       ^ lilypond-ts-font-lock-articulation-face
%%         ^ nil
%%             ^ lilypond-ts-font-lock-phrasing-slur-face
%%               ^ lilypond-ts-font-lock-articulation-face
%%                 ^ nil
  \once\override Voice.NoteHead.stencil = ##f
%%^ font-lock-keyword-face
%%               ^ lilypond-ts-font-lock-context-face
%%                    ^ nil
%%                     ^ font-lock-type-face
%%                             ^ nil
%%                                        ^ font-lock-constant-face
  a4[\startTextSpan 4\^ 4~\stopTextSpan 4:32 |
%% <- nil
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
%%                                          ^ nil
  <c e>2^"div." <c' e,>^\markup\italic { unisono }
%% <- nil
%%     ^ lilypond-ts-font-lock-duration-face
%%      ^ nil
%%       ^ font-lock-string-face
%%             ^ nil
%%                      ^ lilypond-ts-font-lock-markup-keyword-face
%%                             ^ lilypond-ts-font-lock-markup-face
%%                                    ^ nil
  d8(\=2 c[) b a?] gs!2\trill |
%% <- nil
%% ^ lilypond-ts-font-lock-duration-face
%%  ^ lilypond-ts-font-lock-slur-face
%%   ^ font-lock-keyword-face
%%     ^ font-lock-number-face
%%      ^ nil
%%        ^ lilypond-ts-font-lock-beam-face
%%         ^ lilypond-ts-font-lock-slur-face
%%          ^ nil
%%               ^ lilypond-ts-font-lock-beam-face
%%                ^ nil
%%                    ^ lilypond-ts-font-lock-duration-face
%%                     ^ lilypond-ts-font-lock-event-face
%%                           ^ nil
  a\breve*1/2
%% <- nil
%% ^ lilypond-ts-font-lock-duration-face
}
%% <- nil

\score {
%% <- font-lock-keyword-face
%%    ^ nil
  <<
%% <- nil
    \new Staff \with {
%%  ^ font-lock-keyword-face
%%       ^ lilypond-ts-font-lock-context-face
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
%%    ^ lilypond-ts-font-lock-identifier-face
  >>
%% <- nil
  \layout {
%%^ font-lock-keyword-face
%%       ^ nil
    \context {
%%  ^ font-lock-keyword-face
%%          ^ nil
      \Score
%%    ^ lilypond-ts-font-lock-context-face
      %% This is a comment
%%    ^ font-lock-comment-face
      \applyContext #(lambda (ctx)
;;    ^ font-lock-keyword-face
;;                 ^ nil
;;                    ^ font-lock-keyword-face
;;                          ^ nil
                       (message "~a\n bar" (ly:context-children ctx)))
%% <- nil
%%                              ^ font-lock-string-face
%%                                 ^ (font-lock-escape-face font-lock-string-face)
%%                                   ^ font-lock-string-face
%%                                        ^ nil
    }
%% <- nil
  }
%% <- nil
}
%% <- nil
