--------------------------------------------------------------------
S3DE can take advantage of several optimized numerical libraries
(essl, fftw, mkl...).  This configure script attempts to find them,
but may fail if they have been installed in non-standard locations.

The following libraries have been found:
  BLAS_LIBS= -latlas
  LAPACK_LIBS= -llapack
  FFT_LIBS=/Users/marsamos/SaX_svn/SaX/fftw/src/libfftw.a
  @delete@
Please check if that is correct.

If any libraries are missing, you may specify a list of directories
to search and retry, as follows:
  ./configure LIBDIRS="list of directories, separated by spaces"

For more info, read the S3DE documentation.
--------------------------------------------------------------------
