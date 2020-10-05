# SFS (Small Fortran Solvers)

## Origin and Motivation

The embrio of this development started from the algorithms in the book: _Numerical Analysis: Mathematics of Scientific Computing_, by David Kincaid & Ward Cheney.

Starting from sources cordialy provided by the authors, I was converging towards the implementations in _T-Flows_ (https://github.com/DelNov/T-Flows). Initial motivation was to check wheather fill-in makes sense for _ICCG_ solvers, but recenlty I realized that they might also be useful to dive into GPU programming.

## GPU Programming

For GPU programming, I use the OpenACC paradigm and a suite of tools which comes from Nvidia's CUDA toolkit and HPC-SDK.  To be more precise, I used the following combination:

- CUDA toolkit 11.1   (https://developer.nvidia.com/cuda-toolkit)
- Nvidia HPC-SDK 20.7 (https://developer.nvidia.com/hpc-sdk)

(These two don't work in perfect harmony, but if ```LD_LIBRARY_PATH``` points to the directory from CUDA toolkit and not to the libraries which come with HPC-SDK, all seems to work fine.  What will be with next release(s), no one can tell.)

## Hints on Using OpenACC with Nvidia Tools

A couple of hints for compilation with OpenACC.

- Compile with ```-acc -Minfo=accel``` option
- For profiling, use: ```nsys profile --trace=openacc ./SFS <option>``` and later ```launch "nsight-sys``` to load the profile file.

Set environment variable ```NVCOMPILER_ACC_NOTIFY``` to zero if you want to supress messaags while a program runs on GPUs
