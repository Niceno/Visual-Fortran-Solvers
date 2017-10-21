bin_PROGRAMS = SFS

SFS_SOURCES =					\
  Constants_Mod.f90				\
  Compress_Matrix.f90				\
  Create_Matrix_Compressed.f90			\
  Demo_Cholesky_Solver.f90			\
  Demo_Compress_Decompress.f90			\
  Demo_Gauss_Solver.f90				\
  Demo_Incomplete_Cholesky_Solver.f90		\
  Demo_Incomplete_LDLT_Solver.f90		\
  Demo_LDLT_Solver.f90				\
  Demo_LDLT_Solver_From_Tflows.f90		\
  Main.f90					\
  Expand_Matrix.f90

SUBDIRS = Modules		\
          Input_Output		\
          Linear_Algebra	\
          Linear_Solvers

LDADD = Modules/libModules.a			\
        Input_Output/libInput_Output.a		\
        Linear_Algebra/libLinear_Algebra.a	\
        Linear_Solvers/libLinear_Solvers.a

INCLUDE = -I@top_srcdir@/
