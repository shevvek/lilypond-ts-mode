\version "2.25.20"
\language "english"
\include "moment-navigation-2.ily"

global = {
  s1*4
  \time 3/4
  s2.*4
}

A = \relative {
  c'4 d e f
  g a b c
  d c b a
  g f e d
  c4 e g
  f a c
  f d b
  g f d
  c2.
}
