% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indexing.R
\docType{class}
\name{BasicIndexer}
\alias{BasicIndexer}
\title{BasicIndexer Class}
\description{
An indexer class to normalize a selection of an array and provide an iterator
of indexes over the dimensions of an array.
}
\details{
The Zarr BasicIndexer class.
}
\keyword{internal}
\section{Super class}{
\code{pizzarr::Indexer} -> \code{BasicIndexer}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dim_indexers}}{TODO}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BasicIndexer-new}{\code{BasicIndexer$new()}}
\item \href{#method-BasicIndexer-iter}{\code{BasicIndexer$iter()}}
\item \href{#method-BasicIndexer-clone}{\code{BasicIndexer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BasicIndexer-new"></a>}}
\if{latex}{\out{\hypertarget{method-BasicIndexer-new}{}}}
\subsection{Method \code{new()}}{
Create a new VIndex instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasicIndexer$new(selection, array)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{selection}}{selection as with ZarrArray, scalar, string, or Slice. "..." and ":" supported for string}

\item{\code{array}}{ZarrArray object that will be indexed}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{VIndex} instance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BasicIndexer-iter"></a>}}
\if{latex}{\out{\hypertarget{method-BasicIndexer-iter}{}}}
\subsection{Method \code{iter()}}{
An iterator over the dimensions of an array
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasicIndexer$iter()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of ChunkProjection objects
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BasicIndexer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BasicIndexer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasicIndexer$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
