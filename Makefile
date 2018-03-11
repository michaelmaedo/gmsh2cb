#========================================================================#
#                                                                        #
#  Makefile:    G M S H 2 C B                                            #
#               Mark_I                                                   #
#                                                                        #
#========================================================================#

# System Configuration Section
# ============================

# ** Shell **
SHELL = /bin/sh

# ** Suffixes **
.SUFFIXES = .f90 .o .mod

# ** Specifies a list of directories that 'make' should search **
VPATH = src:src/utilities

# ** Compiler **
FC = gfortran -cpp -finit-local-zero -fmax-stack-var-size=30000 -O3 -std=f2003 -Wall

# ** Libraries **
#LIBS = -lblas

# ** Header Files **
INCLUDES = -I/usr/include

# ** Debug **
CDEBUG = -g


# ** Flags **
CFLAGS = -fdefault-double-8 -fno-range-check -Wall

# End of System Configuration Section
# ===================================
OBJS = precision_mod.o parameters_mod.o error_mod.o string_mod.o file_mod.o\
       error_gmsh2cb_mod.o gmsh2cb_mod.o main.o

# ** Rules **
.PHONY: all
all: start

start:
	@echo "\n"
	@echo  " o================================================================o"
	@echo  " |                                                                |"
	@echo  " |                           G M S H 2 C B                        |"
	@echo  " |                          version: Mark 1                       |"
	@echo  " |                                                                |"
	@echo  " |  Consulting : mmaedo@tamu.edu                                  |"
	@echo  " |                                                                |"
	@echo  " o================================================================o"
	@echo "\nCompiling sources"
	@echo   "================="
	@$(MAKE) gmsh2cb.a
	@echo "\nBuilding software"
	@echo   "================="
	@$(MAKE) gmsh2cb
	@echo "\nCleaning up"
	@echo   "==========="
	@$(MAKE) clean
	@echo "\n * * * GMSH2CB was successfully built * * *\n"

gmsh2cb: gmsh2cb.a
	$(FC) $(CFLAGS) -g -o $@ gmsh2cb.a 

gmsh2cb.a: $(OBJS)
	@echo "\nCreating library"
	@echo   "================"
	ar crusv gmsh2cb.a $^

$(OBJS): $(FSRC)

FSRC:
	@FSRC = $(FSRC)

%.o %.mod: %.f90
	$(FC) -g -c $^

.PHONY: clean
clean:
	rm -f *.o *.mod

run:
	./gmsh2cb

# End of the makefile
