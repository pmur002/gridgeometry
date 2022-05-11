\name{NEWS}
\title{NEWS file for the gridGeometry package}
\encoding{UTF-8}

\section{Changes in version 0.3-0}{
  \itemize{
    \item Add dependency on R 4.2.0 because works with more complex
    classes returned by \code{grobCoords}.
    
    \item New \code{xyListFromGrob()} function to reduce grob (or gTree)
    that contains potentially multiple shapes to a single list of sets of
    x/y coordinates (for input to polyclip routines).
    
    \item Makes aliases for \code{xyListPath()} etc named
    \code{xyListToPath()} etc (to match new \code{xyListFromGrob()} function).
  }
}

\section{Changes in version 0.2-0}{
  \itemize{
    \item Add dependency on \pkg{polyclip} 1.10-0 because that now
    supports clipping of "open" polygons (lines).  No more need for
    \pkg{polyclip} fork.
  }
}

\section{Changes in version 0.1-0}{
  \itemize{
    \item Add dependency on R 3.6.0 because require new
    \code{grobCoords} function.
  }
}
