hpxMP
=====

hpxcMP.cpp contains all of the current code for the translation library

omp-tests contains simple openMP programs and the Makefile in omp-tests to build them with OpenUH.
Make buildTests at the top level will do the same thing.

To build with OpenUH build on Hermoine, add /home/jkemp/openUH/bin to your path, or use your own installation of openUH.
I also have "/home/jkemp/openUH/lib/gcc-lib/x86_64-open64-linux/5.0/" appended to my LD_LIBRARY_PATH, though I forget why.

OpenUH can be found here:

http://svn.open64.net/svnroot/open64/branches/OpenUH

Directives not implemented:
threadprivate, copyprivate, and copyin 
ordered 
