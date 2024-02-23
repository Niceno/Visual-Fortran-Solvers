#include "Assert.h90"
#include "Unused.h90"

!==============================================================================!
!   FOUL - The Fortran Output Library
!------------------------------------------------------------------------------!
!   Provides routines enabling Fortran programs to:
!
!   - Write formatted console output using ANSI escape codes
!     (see http://en.wikipedia.org/wiki/ANSI_escape_code
!     for more information)
!   - Convert numbers to strings in a variety of finely-controlled ways,
!     including fully trimmed of whitespace characters
!   - Time program execution and other processes with the highest accuracy
!     provided by the system
!------------------------------------------------------------------------------!
!   Copyright (C) 2010-2011 by Philipp Emanuel Weidmann
!   E-Mail: philipp.weidmann@gmx.de
!------------------------------------------------------------------------------!
!   This library is free software; you can redistribute it and/or modify it 
!   under the terms of the GNU General Public License as published by the      !
!   Free Software Foundation; version 3 of the License.
!
!   This library is distributed in the hope that it will be useful, but 
!   WITHOUT ANY WARRANTY; without even the implied warranty of 
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!
!   See the GNU General Public License for more details.
!==============================================================================!

!==============================================================================!
  module Foul_Mod
!------------------------------------------------------------------------------!
  use Assert_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!

  ! If set to .false., output will consist only of standard text,
  ! allowing the escape characters to be switched off in
  ! environments which don't support them
  logical :: use_escape_codes = .true.

  ! Width (in characters) of the section delimiters
  ! created by start_section and end_section
  integer :: section_width = 60

  ! Unit the output will be written to; the default (6)
  ! indicates the standard output unit, usually the console
  integer :: output_unit = 6

  !---------------!
  !               !
  !   Foul type   !
  !               !
  !---------------!
  type Foul_Type

    contains
      procedure          :: Formatted_Write
      procedure, private :: Get_Escape_Sequence
      procedure          :: Integer_To_String
      procedure, private :: Lower_Case
      procedure, private :: Split_String

  end type

  ! Singleton object
  type(Foul_Type) :: Foul

  contains

#   include "Foul_Mod/Formatted_Write.f90"
#   include "Foul_Mod/Get_Escape_Sequence.f90"
#   include "Foul_Mod/Integer_To_String.f90"
#   include "Foul_Mod/Lower_Case.f90"
#   include "Foul_Mod/Split_String.f90"

  end module Foul_Mod

