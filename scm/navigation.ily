\version "2.24.0"

#(load "navigation.scm")

\layout {
  \context {
    \Score
    \consists Record_locations_translator
    navigationExportProperties = #'(measurePosition
                                    currentBarNumber)
  }
}
