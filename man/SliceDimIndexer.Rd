% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indexing.R
\docType{class}
\name{SliceDimIndexer}
\alias{SliceDimIndexer}
\title{SliceDimIndexer Class}
\description{
TODO
}
\details{
The Zarr SliceDimIndexer class.
}
\keyword{internal}
\section{Super class}{
\code{pizzarr::DimIndexer} -> \code{SliceDimIndexer}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dim_len}}{dimension length}

\item{\code{dim_chunk_len}}{dimension chunk length}

\item{\code{num_chunks}}{number of chunks}

\item{\code{start}}{start}

\item{\code{stop}}{stop}

\item{\code{step}}{step}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SliceDimIndexer-new}{\code{SliceDimIndexer$new()}}
\item \href{#method-SliceDimIndexer-iter}{\code{SliceDimIndexer$iter()}}
\item \href{#method-SliceDimIndexer-clone}{\code{SliceDimIndexer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SliceDimIndexer-new"></a>}}
\if{latex}{\out{\hypertarget{method-SliceDimIndexer-new}{}}}
\subsection{Method \code{new()}}{
Create a new SliceDimIndexer instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SliceDimIndexer$new(dim_sel, dim_len, dim_chunk_len)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dim_sel}}{integer dimension selection}

\item{\code{dim_len}}{integer dimension length}

\item{\code{dim_chunk_len}}{integer dimension chunk length}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{SliceDimIndexer} instance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SliceDimIndexer-iter"></a>}}
\if{latex}{\out{\hypertarget{method-SliceDimIndexer-iter}{}}}
\subsection{Method \code{iter()}}{
TODO
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SliceDimIndexer$iter()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
TODO
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SliceDimIndexer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SliceDimIndexer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SliceDimIndexer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
