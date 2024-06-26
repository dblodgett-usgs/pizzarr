% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stores.R
\docType{class}
\name{MemoryStore}
\alias{MemoryStore}
\title{MemoryStore Class}
\description{
Store class that uses a hierarchy of list objects,
thus all data will be held in main memory.
}
\details{
MemoryStore for Zarr
}
\keyword{internal}
\section{Super class}{
\code{\link[pizzarr:Store]{pizzarr::Store}} -> \code{MemoryStore}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{root}}{The root list for the store.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MemoryStore-new}{\code{MemoryStore$new()}}
\item \href{#method-MemoryStore-get_parent}{\code{MemoryStore$get_parent()}}
\item \href{#method-MemoryStore-get_item}{\code{MemoryStore$get_item()}}
\item \href{#method-MemoryStore-set_item}{\code{MemoryStore$set_item()}}
\item \href{#method-MemoryStore-contains_item}{\code{MemoryStore$contains_item()}}
\item \href{#method-MemoryStore-listdir}{\code{MemoryStore$listdir()}}
\item \href{#method-MemoryStore-rmdir}{\code{MemoryStore$rmdir()}}
\item \href{#method-MemoryStore-clone}{\code{MemoryStore$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="close"><a href='../../pizzarr/html/Store.html#method-Store-close'><code>pizzarr::Store$close()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="get_consolidated_metadata"><a href='../../pizzarr/html/Store.html#method-Store-get_consolidated_metadata'><code>pizzarr::Store$get_consolidated_metadata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="is_erasable"><a href='../../pizzarr/html/Store.html#method-Store-is_erasable'><code>pizzarr::Store$is_erasable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="is_listable"><a href='../../pizzarr/html/Store.html#method-Store-is_listable'><code>pizzarr::Store$is_listable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="is_readable"><a href='../../pizzarr/html/Store.html#method-Store-is_readable'><code>pizzarr::Store$is_readable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="is_writeable"><a href='../../pizzarr/html/Store.html#method-Store-is_writeable'><code>pizzarr::Store$is_writeable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="pizzarr" data-topic="Store" data-id="rename"><a href='../../pizzarr/html/Store.html#method-Store-rename'><code>pizzarr::Store$rename()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-new"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-new}{}}}
\subsection{Method \code{new()}}{
Create a new memory store.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$new()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{MemoryStore} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-get_parent"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-get_parent}{}}}
\subsection{Method \code{get_parent()}}{
Get the parent of an item.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$get_parent(item)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{item}}{The item key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list with the keys \code{parent} and \code{key}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-get_item"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-get_item}{}}}
\subsection{Method \code{get_item()}}{
Get an item from the store.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$get_item(item = NA)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{item}}{The item key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The item data in a vector of type raw.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-set_item"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-set_item}{}}}
\subsection{Method \code{set_item()}}{
Set an item in the store.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$set_item(item, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{item}}{The item key.}

\item{\code{value}}{The item value as a vector of type raw.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-contains_item"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-contains_item}{}}}
\subsection{Method \code{contains_item()}}{
Determine whether the store contains an item.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$contains_item(item)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{item}}{The item key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A boolean value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-listdir"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-listdir}{}}}
\subsection{Method \code{listdir()}}{
list the store directory
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$listdir(key = NA)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{character key}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-rmdir"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-rmdir}{}}}
\subsection{Method \code{rmdir()}}{
remove a path within a Store
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$rmdir(item)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{item}}{character item}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MemoryStore-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MemoryStore-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MemoryStore$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
