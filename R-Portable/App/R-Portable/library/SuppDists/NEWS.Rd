\name{NEWS}
\title{SuppDists News}
\encoding{UTF-8}
\section{Changes in version 1.1-9.7 (2022-01-02)}{
  \itemize{
    \item The file \sQuote{Makevars} was added with flags 
    \code{CXX_STD = CXX11}.
  }
}
\section{Changes in version 1.1-9.6 (2020-01-18)}{
  \itemize{
    \item Cpp code of \sQuote{MWC1019R} and \sQuote{ziggR} 
    was disabled.
  }
}
\section{Changes in version 1.1-9.5 (2020-01-13)}{
  \itemize{
  \item The package that was written by Robert E. Wheeler
  has been orphaned since 2013. It was scheduled for 
  archival by the CRAN team, as the package lacked of an
  active maintenance and \pkg{SuppDists v1.1-9.4} had
  three issues acording to CRAN CHECK, 
  namely no calls to: \sQuote{R_registerRoutines}, 
  \sQuote{R_useDynamicSymbols}, an explicit 
   self-assignment of the value of variable of type 
   \sQuote{double} to itself and memory leaks due to 
   uninitialised variables that were detected by \code{valgrind}.
     \item Initial writing of \code{init.cpp} with calls to
     \code{R_registerRoutines} and 
     \code{R_useDynamicSymbols}
     \item Change the \code{.C(...)} calls in R from 
     \code{character} to symbols.
     \item Fix of self-assignment.
      \item The two RNG functions \code{rziggurat} and 
      \code{rMWC1019} were defunct.
     \item Explicit exporting of R functions in NAMESPACE.
     \item Update of DESCRIPTION file.
     \item Orphant \code{email} in \code{author} sections of
     Rd files were removed.
  }
}
