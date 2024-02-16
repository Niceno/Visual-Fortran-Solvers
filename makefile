#===============================================================================
#
#   VFS (Visual Fortran Solvers) Makefile
#
#-------------------------------------------------------------------------------

#-------------------------------------
#   Default options for compilation
#-------------------------------------
FORTRAN ?= gnu
DEBUG   ?= no
ASSERT  ?= yes

#--------------------------
#   Variable definitions
#--------------------------

# Directories for objects and modules. (No need to change.)
DIR_BINARY = .
DIR_MODULE = .Modules
DIR_OBJECT = .Objects

# Program name (This should hardly change)
PROGRAM_NAME = VFS
PROGRAM_FILE = $(DIR_BINARY)/$(PROGRAM_NAME)

#-------------------------------------------
#   Variables to pass to compiled program
#-------------------------------------------
PASS_ON = -DVFS=1
ifeq ($(ASSERT), yes)
  PASS_ON += -DVFS_ASSERT=1
else
  PASS_ON += -DVFS_ASSERT=0
endif

$(info #=======================================================================)
$(info # Compiling $(PROGRAM_NAME) with compiler $(FORTRAN))
$(info #-----------------------------------------------------------------------)
$(info # Usage:                                                                )
$(info #   make <FORTRAN=gnu/intel/nvidia> <ASSERT=yes/no> <DEBUG=no/yes>      )
$(info #                                                                       )
$(info # Examples:                                                             )
$(info #   make              - compile with gnu compiler                       )
$(info #   make FORTAN=intel - compile with intel compiler                     )
$(info #   make DEBUG=yes    - compile with gnu compiler in debug mode         )
$(info #-----------------------------------------------------------------------)

#-------------------------------------------------------------------------------
#   Compiler and linker options
#-------------------------------------------------------------------------------
#   Note: Changes only when support to a new Fortran compiler is added.
#-------------------------------------------------------------------------------
 
# Fortran == gnu
ifeq ($(FORTRAN), gnu)
  $(info  # Using GNU Fortran compiler with options:)
  FC = gfortran
  ifeq ($(DEBUG),yes)
    OPT_COMP = -J $(DIR_MODULE) -fdefault-real-8 -fdefault-integer-8 -O0 -g  \
                                -Wunused-parameter -Wall                     \
                                -cpp
  else
    OPT_COMP = -J $(DIR_MODULE) -fdefault-real-8 -fdefault-integer-8 -O3 -cpp
  endif 
  OPT_LINK = $(OPT_COMP)
endif 

# Fortran == intel
ifeq ($(FORTRAN), intel)
  $(info  # Using Intel Fortran compiler with options:)
  FC = ifort
  ifeq ($(DEBUG),yes)
    OPT_COMP = -module $(DIR_MODULE) -r8 -i8 -O0 -g -warn all -check all \
               -debug all -fpe-all=0 -traceback -cpp
  else
    OPT_COMP = -module $(DIR_MODULE) -r8 -i8 -O3 -cpp
  endif
  OPT_LINK = $(OPT_COMP)
endif 

# Fortran == nvidia
ifeq ($(FORTRAN), nvidia)
  $(info  # Using Nvidia Fortran compiler with options:)
  FC = nvfortran
  ifeq ($(DEBUG),yes)
    OPT_COMP = -module $(DIR_MODULE) -r8 -i8 -O0 -g -cpp
  else
    OPT_COMP = -module $(DIR_MODULE) -r8 -i8 -O3 -cpp
  endif
  OPT_LINK = $(OPT_COMP)
endif

$(info  $(OPT_COMP))

#------------------------------------------------------
#   List of sources for modules and functions
#------------------------------------------------------
#   Modules' order must obey their dependency 
#   This list should therefore be written "by hand".
#   Note: Modules written in lower case 
#         letters are candidates for deletion.
#------------------------------------------------------

#-------------
#   Modules
#-------------

# Modules in shared directories
SRC_MOD = Assert_Mod.f90	\
          Foul_Mod.f90		\
          Sort_Mod.f90		\
          Grid_Mod.f90		\
          Dense_Mod.f90		\
          Sparse_Mod.f90	\
          In_Out_Mod.f90	\
          Lin_Alg_Mod.f90	\
          Discretize_Mod.f90	\
          Solvers_Mod.f90	\
          Demo_Mod.f90

#---------------
#   Functions
#---------------

# Sources for all functions are obtained by a shell command
SRC_FUN = $(shell ls -1 *.f90			\
                        | xargs -n1 basename	\
                        | grep -v -i _Mod)

#----------------------------------------------------------------------
#   List of objects generated from the list of modules and functions
#----------------------------------------------------------------------
#   Note: This doesn't need editing.
#----------------------------------------------------------------------
OBJ_MOD = $(SRC_MOD:%.f90=$(DIR_OBJECT)/%.o)
OBJ_FUN = $(SRC_FUN:%.f90=$(DIR_OBJECT)/%.o)
OBJ = $(OBJ_MOD) $(OBJ_FUN)

#---------------------------------------------------------
#   Default rule to build Fortran modules and functions
#---------------------------------------------------------
#   Note: This doesn't need editing.
#---------------------------------------------------------

# Modules
$(DIR_OBJECT)/%.o: %.f90 %/*.f90 makefile*
	@echo FC $<
	@$(FC) $(OPT_COMP) $(PASS_ON) -c -o $@ $<

# Functions
$(DIR_OBJECT)/%.o: %.f90 makefile*
	@echo FC $<
	@$(FC) $(OPT_COMP) $(PASS_ON) -c -o $@ $<

#-----------------------------------
#   Rule to build main program
#-----------------------------------
#   Note: Should not be modified.
#-----------------------------------
$(PROGRAM_FILE): $(OBJ)
	@echo Linking "\033[0;32m $(PROGRAM_FILE) \033[0m"
	@$(FC) $(OPT_LINK) -o $(PROGRAM_FILE) $(OBJ)

#--------------------------------------------------------------------
#   Explicit dependencies for modules
#--------------------------------------------------------------------
#   These are automatically generated by:
#   Sources/Utilities/create_external_dependencies_for_makefile.sh
#--------------------------------------------------------------------
include makefile_explicit_dependencies

#---------------------
#   Explicit target.
#---------------------
clean:
	rm -f $(DIR_OBJECT)/*.o $(DIR_MODULE)/*.mod $(PROGRAM_FILE)
