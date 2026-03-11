# pizzarr 0.1.0

* Initial release of pizzarr.
* Read and write Zarr V2 and Zarr V3 stores.
* Storage backends: DirectoryStore, MemoryStore, HttpStore.
* Compression codecs: Zstd, LZ4, Blosc, zlib, gzip, bzip2, LZMA.
* VLenUTF8 object codec for variable-length strings.
* Parallel read/write support via pbapply, parallel, and future.
* R-like one-based and Python-like zero-based slicing.
