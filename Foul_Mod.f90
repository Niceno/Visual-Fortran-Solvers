!------------------------------------------------------------
! foul - The Fortran Output Library
!------------------------------------------------------------
! Provides routines enabling Fortran programs to:
!
! - Write formatted console output using ANSI escape codes
!   (see http://en.wikipedia.org/wiki/ANSI_escape_code
!    for more information)
! - Convert numbers to strings in a variety of
!   finely-controlled ways, including fully trimmed
!   of whitespace characters
! - Time program execution and other processes with the
!   highest accuracy provided by the system
!------------------------------------------------------------
! Copyright (C) 2010-2011 by Philipp Emanuel Weidmann
! E-Mail: philipp.weidmann@gmx.de
!------------------------------------------------------------
! This library is free software; you can redistribute it
! and/or modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! version 3 of the License.
!
! This library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied
! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
! PURPOSE.
!
! See the GNU General Public License for more details.
!------------------------------------------------------------
! Version history:
!
! 0.7 (18/02/2011): - Added real_to_string_scientific_short,
!                     a function for formatting numbers in
!                     classic Fortran scientific notation
!                   - Added iif_* routines emulating the
!                     behaviour of C++'s iif
!                   - Added handling of NaN values to
!                     real_to_string_* functions
!                   - Fixed a rare bug where rounding
!                     changes the number of digits before
!                     the decimal seperator in
!                     real_to_string
!
! 0.6 (02/07/2010): - Added support for multiple timers
!                     that can be used concurrently
!                   - Improved modularization
!                   - Moved module foul_helpers to the
!                     start of the code. This is in
!                     compliance with the Fortran standard
!                     and should fix compatibility issues
!                     with older versions of GFortran
!                     (reported by Arjen Markus)
!
! 0.5 (08/06/2010): - Added start_timer, stop_timer,
!                     reset_timer, get_timer_count,
!                     functions for measuring execution
!                     times
!                   - Added formatting for section headers
!                     and footers (experimental)
!                   - Fixed a bug in get_escape_sequence
!                     that prevented certain style strings
!                     from being interpreted correctly
!                   - The output unit is now a global
!                     variable, making it easy to write
!                     to units other than the console
!
! 0.4 (06/06/2010): - Major rewrite of write_formatted,
!                     fixing all known bugs related to
!                     string length and whitespace issues
!                     and finally introducing
!                     FULL GFORTRAN COMPATIBILITY
!                     (Note: This breaks compatibility
!                            with calls made to previous
!                            versions of write_formatted)
!                   - Minor improvements and fixes in
!                     real_to_string_scientific
!
! 0.3 (04/06/2010): - Added real_to_string and
!                     real_to_string_scientific, powerful
!                     functions for formatting real values
!                   - Fixed an important bug in
!                     integer_to_string that would sometimes
!                     cause it to return an empty string
!                   - Other minor improvements and fixes
!
! 0.2 (01/06/2010): - Assumed-shape arrays used in
!                     write_formatted, eliminating the need
!                     to supply the array size
!                     (Note: This breaks compatibility
!                            with calls made to previous
!                            versions of write_formatted)
!                   - Reduced redundancy by introducing
!                     integer_to_string
!
! 0.1 (30/05/2010): - Initial release
!                   - Note: DOES NOT WORK WITH GFORTRAN YET;
!                           IFC is the only compiler
!                           confirmed to compile the example
!                           (string array length issue)
!------------------------------------------------------------



MODULE foul_helpers

CONTAINS



! TODO: Turn into FUNCTION?
!------------------------------------------------------------
! SUBROUTINE get_escape_sequence
!
! Helper function
! Generates an ANSI escape sequence from the
! supplied style string
!------------------------------------------------------------
! Input:
! style_string: String describing which styles to set
!               (separated by space);
!               see source code for supported styles
!
! Output:
! escape_sequence: ANSI escape sequence generated from
!                  the specified styles
!------------------------------------------------------------
SUBROUTINE get_escape_sequence(style_string, escape_sequence)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(IN)  :: style_string
    CHARACTER * 16,      INTENT(OUT) :: escape_sequence

    INTEGER :: i

    CHARACTER * 32 :: style_substrings(16)
    INTEGER        :: style_substring_count


    ! Start sequence with command to clear any previous attributes
    escape_sequence = CHAR(27) // '[0'

    CALL split_string(TRIM(style_string), ' ', style_substrings, style_substring_count)

    DO i = 1, style_substring_count
        CALL lower_case(style_substrings(i))

        SELECT CASE (TRIM(style_substrings(i)))
        CASE ('bright')
            escape_sequence = TRIM(escape_sequence) // ';1'
        CASE ('faint')
            escape_sequence = TRIM(escape_sequence) // ';2'
        CASE ('italic')
            escape_sequence = TRIM(escape_sequence) // ';3'
        CASE ('underline')
            escape_sequence = TRIM(escape_sequence) // ';4'
        CASE ('blink_slow')
            escape_sequence = TRIM(escape_sequence) // ';5'
        CASE ('blink_fast')
            escape_sequence = TRIM(escape_sequence) // ';6'
        CASE ('black')
            escape_sequence = TRIM(escape_sequence) // ';30'
        CASE ('red')
            escape_sequence = TRIM(escape_sequence) // ';31'
        CASE ('green')
            escape_sequence = TRIM(escape_sequence) // ';32'
        CASE ('yellow')
            escape_sequence = TRIM(escape_sequence) // ';33'
        CASE ('blue')
            escape_sequence = TRIM(escape_sequence) // ';34'
        CASE ('magenta')
            escape_sequence = TRIM(escape_sequence) // ';35'
        CASE ('cyan')
            escape_sequence = TRIM(escape_sequence) // ';36'
        CASE ('white')
            escape_sequence = TRIM(escape_sequence) // ';37'
        CASE ('background_black')
            escape_sequence = TRIM(escape_sequence) // ';40'
        CASE ('background_red')
            escape_sequence = TRIM(escape_sequence) // ';41'
        CASE ('background_green')
            escape_sequence = TRIM(escape_sequence) // ';42'
        CASE ('background_yellow')
            escape_sequence = TRIM(escape_sequence) // ';43'
        CASE ('background_blue')
            escape_sequence = TRIM(escape_sequence) // ';44'
        CASE ('background_magenta')
            escape_sequence = TRIM(escape_sequence) // ';45'
        CASE ('background_cyan')
            escape_sequence = TRIM(escape_sequence) // ';46'
        CASE ('background_white')
            escape_sequence = TRIM(escape_sequence) // ';47'
        END SELECT
    END DO

    ! Append end of sequence marker
    escape_sequence = TRIM(escape_sequence) // 'm'


    RETURN
END SUBROUTINE get_escape_sequence



!------------------------------------------------------------
! SUBROUTINE split_string
!
! Helper function
! Splits the supplied string along a delimiter
!------------------------------------------------------------
! Input:
! string:    Variable-length character string that is
!            to be split
! delimiter: Character along which to split
!
! Output:
! substrings:      Array of substrings generated by split
!                  operation
! substring_count: Number of substrings generated
!------------------------------------------------------------
SUBROUTINE split_string(string, delimiter, substrings, substring_count)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(IN)  :: string
    CHARACTER,           INTENT(IN)  :: delimiter
    CHARACTER (LEN = *), INTENT(OUT) :: substrings(*)
    INTEGER,             INTENT(OUT) :: substring_count

    INTEGER :: start_position, end_position


    start_position  = 1
    substring_count = 0

    DO
        end_position = INDEX(string(start_position:), delimiter)

        substring_count = substring_count + 1

        IF (end_position == 0) THEN
            substrings(substring_count) = string(start_position:)
            EXIT
        ELSE
            substrings(substring_count) = string(start_position : start_position + end_position - 2)
            start_position = start_position + end_position
        END IF
    END DO


    RETURN
END SUBROUTINE split_string



! TODO: Turn into FUNCTION
!------------------------------------------------------------
! SUBROUTINE lower_case
!
! Helper function
! Converts the supplied string to lower case
!------------------------------------------------------------
! Input:
! string: Variable-length character string that is
!         to be converted
!
! Output:
! string: String converted to lower case; conversion
!         is done in place
!------------------------------------------------------------
SUBROUTINE lower_case(string)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(INOUT) :: string

    INTEGER :: i, character_code


    DO i = 1, LEN_TRIM(string)
        character_code = ICHAR(string(i : i))

        IF (character_code >= 65 .AND. character_code <= 90) THEN
            string(i : i) = CHAR(character_code + 32)
        END IF
    END DO


    RETURN
END SUBROUTINE lower_case



!------------------------------------------------------------
! FUNCTION integer_to_string
!
! Converts the supplied integer to a character string
!------------------------------------------------------------
! Input:
! integer_value: Value to be converted
! string_length: Minimal length of resulting string;
!                result will be right-adjusted if desired
!                length is greater than actual length
!
! Output:
! Return value: String resulting from conversion
!------------------------------------------------------------
FUNCTION integer_to_string(integer_value, string_length)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: integer_value, string_length

    ! 1 + FLOOR(LOG10(REAL(ABS(integer_value)))) =
    ! number of digits needed to represent integer_value
    !
    ! INT(SIGN(1, -integer_value) + 1) / 2) =
    ! 0, if integer_value is positive
    ! 1, if integer_value is negative
    ! (used to make room for sign)
    CHARACTER (LEN = MAX(1 + FLOOR(LOG10(REAL(ABS(integer_value)))) +   &
                         INT((SIGN(1, -integer_value) + 1) / 2),           &
                         string_length, 1)) :: integer_to_string

    CHARACTER * 16 :: string_buffer


    WRITE(string_buffer, '(I16)') integer_value
    string_buffer = ADJUSTL(string_buffer)

    IF (LEN_TRIM(string_buffer) < LEN(integer_to_string)) THEN
        integer_to_string = REPEAT(' ', LEN(integer_to_string) - LEN_TRIM(string_buffer)) //   &
                            TRIM(string_buffer)
    ELSE
        integer_to_string = TRIM(string_buffer)
    END IF


    RETURN
END FUNCTION integer_to_string



!------------------------------------------------------------
! FUNCTION real_to_string
!
! Converts the supplied real to a character string
!------------------------------------------------------------
! Input:
! real_value:        Value to be converted
! integer_length:    Minimal length of the integral part in
!                    the resulting string; result will be
!                    right-adjusted if desired length is
!                    greater than actual length
! fractional_length: Length of the fractional part
!                    in the resulting string
!
! Output:
! Return value: String resulting from conversion
!------------------------------------------------------------
FUNCTION real_to_string(real_value, integer_length, fractional_length)
    IMPLICIT NONE

    REAL * 8, INTENT(IN) :: real_value
    INTEGER,  INTENT(IN) :: integer_length, fractional_length

    ! 1 + FLOOR(LOG10(ABS(real_value))) =
    ! number of digits needed to represent integral part of real_value
    !
    ! INT(SIGN(1.0, -real_value) + 1) / 2) =
    ! 0, if real_value is positive
    ! 1, if real_value is negative
    ! (used to make room for sign)
    CHARACTER (LEN = MAX(1 + FLOOR(LOG10(ABS(real_value))) +     &
                         INT((SIGN(1.0, -real_value) + 1) / 2),   &
                         integer_length, 1) + 1 + fractional_length) :: real_to_string

    INTEGER        :: digit_count

    CHARACTER * 16 :: format_string
    CHARACTER * 32 :: string_buffer


    IF (ISNAN(real_value)) THEN
        real_to_string = REPEAT(' ', LEN(real_to_string) - 3) // 'NaN'
        RETURN
    END IF


    IF (real_value == 0.0) THEN
        ! Filter out zero to prevent additional digit (for sign)
        ! from being introduced (because SIGN(1.0, -0.0) = 1.0)
        digit_count = 1
    ELSE IF (ABS(real_value) < 1) THEN
        ! Using the logarithm to determine number of digits
        ! doesn't work for values < 1
        digit_count = INT(((SIGN(1.0, -real_value) + 1) / 2)) + 1
    ELSE
        digit_count = INT(((SIGN(1.0, -real_value) + 1) / 2)) + 1 +   &
                      FLOOR(LOG10(ABS(real_value)))
    END IF

    format_string = '(F' // integer_to_string(digit_count + 1 + fractional_length, 0) //   &
                    '.' // integer_to_string(fractional_length, 0) // ')'

    ! Truncation is neccessary in order to catch a rare bug
    ! where rounding would change the numer of digits in front
    ! of the decimal separator
!    WRITE(string_buffer, TRIM(format_string)) DBLE(INT(real_value * (10.0**fractional_length))) /   &
!                                              (10.0**fractional_length)
    WRITE(string_buffer, TRIM(format_string)) real_value

    string_buffer = ADJUSTL(string_buffer)

    IF (LEN_TRIM(string_buffer) < LEN(real_to_string)) THEN
        real_to_string = REPEAT(' ', LEN(real_to_string) - LEN_TRIM(string_buffer)) //   &
                         TRIM(string_buffer)
    ELSE
        real_to_string = TRIM(string_buffer)
    END IF


    RETURN
END FUNCTION real_to_string



!------------------------------------------------------------
! FUNCTION real_to_string_scientific
!
! Converts the supplied real to a character string
! in scientific notation
!------------------------------------------------------------
! Input:
! real_value:        Value to be converted
! integer_length:    Minimal length of the integral part in
!                    the resulting string; result will be
!                    right-adjusted if desired length is
!                    greater than actual length
! fractional_length: Length of the fractional part
!                    in the resulting string
! exponent_length:   Minimal length of the exponent part
!                    in the resulting string
!
! Output:
! Return value: String resulting from conversion
!------------------------------------------------------------
FUNCTION real_to_string_scientific(real_value, integer_length, fractional_length, exponent_length)
    IMPLICIT NONE

    REAL * 8, INTENT(IN) :: real_value
    INTEGER,  INTENT(IN) :: integer_length, fractional_length, exponent_length

    ! TODO: Correct exponent length!!!
    CHARACTER (LEN = MAX(1 + INT((SIGN(1.0, -real_value) + 1) / 2), integer_length) +   &
                     1 + fractional_length + 7 + exponent_length + 1) :: real_to_string_scientific

    INTEGER  :: exponent
    REAL * 8 :: coefficient


    IF (ISNAN(real_value)) THEN
        real_to_string_scientific = REPEAT(' ', LEN(real_to_string_scientific) - 3) // 'NaN'
        RETURN
    END IF


    IF (ABS(real_value) == 0.0) THEN
        ! No exponent neccessary
        exponent = 0
    ELSE
        exponent = FLOOR(LOG10(ABS(real_value)))
    END IF

    coefficient = real_value / (10.0 ** REAL(exponent))


    IF (exponent == 0) THEN
        real_to_string_scientific = real_to_string(coefficient, integer_length, fractional_length) //   &
                                    REPEAT(' ', 7 + exponent_length + 1)
    ELSE
        real_to_string_scientific = real_to_string(coefficient, integer_length, fractional_length) //   &
                                    ' x (10^' // integer_to_string(exponent, exponent_length) // ')'
    END IF


    RETURN
END FUNCTION real_to_string_scientific



!------------------------------------------------------------
! FUNCTION real_to_string_scientific_short
!
! Converts the supplied real to a character string
! in scientific notation using "pocket calculator"
! short form (e.g. "3.2E6")
!------------------------------------------------------------
! Input:
! real_value:        Value to be converted
! integer_length:    Minimal length of the integral part in
!                    the resulting string; result will be
!                    right-adjusted if desired length is
!                    greater than actual length
! fractional_length: Length of the fractional part
!                    in the resulting string
! exponent_length:   Minimal length of the exponent part
!                    in the resulting string
!
! Output:
! Return value: String resulting from conversion
!------------------------------------------------------------
FUNCTION real_to_string_scientific_short(real_value, integer_length,   &
                                         fractional_length, exponent_length)
    IMPLICIT NONE

    REAL * 8, INTENT(IN) :: real_value
    INTEGER,  INTENT(IN) :: integer_length, fractional_length, exponent_length

    ! TODO: Correct exponent length!!!
    CHARACTER (LEN = MAX(1 + INT((SIGN(1.0, -real_value) + 1) / 2), integer_length) +   &
                     1 + fractional_length + 1 + exponent_length) :: real_to_string_scientific_short

    INTEGER  :: exponent
    REAL * 8 :: coefficient

    CHARACTER (LEN = exponent_length) :: exponent_string


    IF (ISNAN(real_value)) THEN
        real_to_string_scientific_short = REPEAT(' ', LEN(real_to_string_scientific_short) - 3) // 'NaN'
        RETURN
    END IF


    IF (ABS(real_value) == 0.0) THEN
        ! No exponent neccessary
        exponent = 0
    ELSE
        exponent = FLOOR(LOG10(ABS(real_value)))
    END IF

    coefficient = real_value / (10.0 ** REAL(exponent))


    IF (exponent == 0) THEN
        real_to_string_scientific_short = REPEAT(' ', 1 + exponent_length) //   &
                                          real_to_string(coefficient, integer_length, fractional_length)

    ELSE
        IF (exponent < 0) THEN
            exponent_string = integer_to_string(-exponent, 0)
            exponent_string = '-' // REPEAT('0', exponent_length - LEN_TRIM(exponent_string) - 1) //   &
                              exponent_string
        ELSE
            exponent_string = integer_to_string(exponent, 0)
            exponent_string = REPEAT('0', exponent_length - LEN_TRIM(exponent_string)) //   &
                              exponent_string
        END IF

        real_to_string_scientific_short = real_to_string(coefficient, integer_length, fractional_length) //   &
                                          'E' // exponent_string
    END IF


    RETURN
END FUNCTION real_to_string_scientific_short



END MODULE foul_helpers



!------------------------------------------------------------



MODULE foul_iif

    INTERFACE iif
        MODULE PROCEDURE iif_integer
        MODULE PROCEDURE iif_real
        MODULE PROCEDURE iif_string
    END INTERFACE

CONTAINS



!------------------------------------------------------------
! FUNCTION iif_integer
!
! Returns one of two integer values depending on
! how an expression evaluates
!------------------------------------------------------------
! Input:
! expression:  Boolean-valued expression to test for
! true_value:  Value to return if expression evaluates
!              to .TRUE.
! false_value: Value to return if expression evaluates
!              to .FALSE.
!
! Output:
! Return value: true_value or false_value (see above)
!------------------------------------------------------------
INTEGER FUNCTION iif_integer(expression, true_value, false_value)
    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: expression
    INTEGER, INTENT(IN) :: true_value, false_value


    IF (expression) THEN
        iif_integer = true_value
    ELSE
        iif_integer = false_value
    END IF


    RETURN
END FUNCTION iif_integer



!------------------------------------------------------------
! FUNCTION iif_real
!
! Returns one of two real values depending on
! how an expression evaluates
!------------------------------------------------------------
! Input:
! expression:  Boolean-valued expression to test for
! true_value:  Value to return if expression evaluates
!              to .TRUE.
! false_value: Value to return if expression evaluates
!              to .FALSE.
!
! Output:
! Return value: true_value or false_value (see above)
!------------------------------------------------------------
REAL * 8 FUNCTION iif_real(expression, true_value, false_value)
    IMPLICIT NONE

    LOGICAL,  INTENT(IN) :: expression
    REAL * 8, INTENT(IN) :: true_value, false_value


    IF (expression) THEN
        iif_real = true_value
    ELSE
        iif_real = false_value
    END IF


    RETURN
END FUNCTION iif_real



!------------------------------------------------------------
! FUNCTION iif_string
!
! Returns one of two string values depending on
! how an expression evaluates
!------------------------------------------------------------
! Input:
! expression:  Boolean-valued expression to test for
! true_value:  Value to return if expression evaluates
!              to .TRUE.
! false_value: Value to return if expression evaluates
!              to .FALSE.
!
! Output:
! Return value: true_value or false_value (see above)
!------------------------------------------------------------
FUNCTION iif_string(expression, true_value, false_value)
    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: expression
    CHARACTER (LEN = *), INTENT(IN) :: true_value, false_value

    CHARACTER (LEN = MAX(LEN(true_value), LEN(false_value))) :: iif_string


    IF (expression) THEN
        iif_string = true_value
    ELSE
        iif_string = false_value
    END IF


    RETURN
END FUNCTION iif_string



END MODULE foul_iif



!------------------------------------------------------------



MODULE foul_timing



    IMPLICIT NONE



    INTEGER, PARAMETER :: maximum_timers = 256

    LOGICAL :: timer_running(maximum_timers)    = .FALSE.

    REAL    :: timer_start_time(maximum_timers) = 0.0
    REAL    :: timer_count(maximum_timers)      = 0.0



CONTAINS



!------------------------------------------------------------
! FUNCTION get_time
!
! Helper function
! Returns the most accurate time value
! available on the system
!------------------------------------------------------------
! Input:
! None
!
! Output:
! Return value: CPU / system time in seconds
!------------------------------------------------------------
REAL FUNCTION get_time()
    IMPLICIT NONE

    REAL    :: processor_time
    INTEGER :: system_time, cycles_per_second


    ! Supported by Fortran 95 and later
    ! according to the PGI Fortran Reference
    CALL CPU_TIME(processor_time)

    IF (processor_time == -1.0) THEN
        ! CPU time not available
        ! => try obtaining system time
        CALL SYSTEM_CLOCK(system_time, cycles_per_second)

        IF (cycles_per_second == 0) THEN
            ! System time not available
            get_time = 0.0
        ELSE
            get_time = REAL(system_time) / REAL(cycles_per_second)
        END IF

    ELSE
        get_time = processor_time
    END IF


    RETURN
END FUNCTION get_time



!------------------------------------------------------------
! SUBROUTINE start_timer
!
! (Re)starts a timer; if reset_timer wasn't
! called before, the count will resume from the value
! that the timer last stopped at
!------------------------------------------------------------
! Input:
! timer_id: ID of the timer to start
!           (OPTIONAL; defaults to 1)
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE start_timer(timer_id)
    IMPLICIT NONE

    INTEGER, INTENT(IN), OPTIONAL :: timer_id
    INTEGER                       :: timer_id_dummy


    IF (PRESENT(timer_id)) THEN
        timer_id_dummy = timer_id
    ELSE
        ! Default to first timer
        timer_id_dummy = 1
    END IF


    IF (.NOT. timer_running(timer_id_dummy)) THEN
        timer_running(timer_id_dummy)    = .TRUE.
        timer_start_time(timer_id_dummy) = get_time()
    END IF


    RETURN
END SUBROUTINE start_timer



!------------------------------------------------------------
! SUBROUTINE stop_timer
!
! Stops a timer pending reset or restart
!------------------------------------------------------------
! Input:
! timer_id: ID of the timer to stop
!           (OPTIONAL; defaults to 1)
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE stop_timer(timer_id)
    IMPLICIT NONE

    INTEGER, INTENT(IN), OPTIONAL :: timer_id
    INTEGER                       :: timer_id_dummy


    IF (PRESENT(timer_id)) THEN
        timer_id_dummy = timer_id
    ELSE
        ! Default to first timer
        timer_id_dummy = 1
    END IF


    IF (timer_running(timer_id_dummy)) THEN
        timer_running(timer_id_dummy) = .FALSE.

        ! Add time elapsed since timer start to timer count
        timer_count(timer_id_dummy) = timer_count(timer_id_dummy) +   &
                                      (get_time() - timer_start_time(timer_id_dummy))
    END IF


    RETURN
END SUBROUTINE stop_timer



!------------------------------------------------------------
! SUBROUTINE reset_timer
!
! Resets the count of a timer
!------------------------------------------------------------
! Input:
! timer_id: ID of the timer to reset
!           (OPTIONAL; defaults to 1)
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE reset_timer(timer_id)
    IMPLICIT NONE

    INTEGER, INTENT(IN), OPTIONAL :: timer_id
    INTEGER                       :: timer_id_dummy


    IF (PRESENT(timer_id)) THEN
        timer_id_dummy = timer_id
    ELSE
        ! Default to first timer
        timer_id_dummy = 1
    END IF


    ! Leaving timer running on reset makes no sense
    ! and has the potential to create problems
    timer_running    = .FALSE.

    timer_start_time = 0.0
    timer_count      = 0.0


    RETURN
END SUBROUTINE reset_timer



!------------------------------------------------------------
! FUNCTION get_timer_count
!
! Returns the current count of a timer
!------------------------------------------------------------
! Input:
! timer_id: ID of the timer to query
!           (OPTIONAL; defaults to 1)
!
! Output:
! Return value: Timer count in seconds
!------------------------------------------------------------
REAL FUNCTION get_timer_count(timer_id)
    IMPLICIT NONE

    INTEGER, INTENT(IN), OPTIONAL :: timer_id
    INTEGER                       :: timer_id_dummy


    IF (PRESENT(timer_id)) THEN
        timer_id_dummy = timer_id
    ELSE
        ! Default to first timer
        timer_id_dummy = 1
    END IF


    IF (timer_running(timer_id_dummy)) THEN
        ! Timer running => add time elapsed since timer
        !                  start to returned timer count
        get_timer_count = timer_count(timer_id_dummy) +   &
                          (get_time() - timer_start_time(timer_id_dummy))
    ELSE
        get_timer_count = timer_count(timer_id_dummy)
    END IF


    RETURN
END FUNCTION get_timer_count



END MODULE foul_timing



!------------------------------------------------------------



MODULE Foul_Mod

    USE foul_helpers
    USE foul_iif
    USE foul_timing

    IMPLICIT NONE

    ! If set to .FALSE., output will consist only of standard text,
    ! allowing the escape characters to be switched off in
    ! environments which don't support them
    LOGICAL :: use_escape_codes = .TRUE.

    ! Width (in characters) of the section delimiters
    ! created by start_section and end_section
    INTEGER :: section_width = 60

    ! Unit the output will be written to; the default (6)
    ! indicates the standard output unit, usually the console
    INTEGER :: output_unit = 6



CONTAINS



!------------------------------------------------------------
! SUBROUTINE write_formatted
!
! Outputs the supplied strings on a single line,
! formatted using the supplied styles
!------------------------------------------------------------
! Input:
! text_1:  First string to output
! style_1: String describing which styles to set when
!          outputting text_1 (separated by space);
!          see get_escape_sequence for supported styles
! [...] (up to 24 text strings supported)
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE write_formatted(text_1,  style_1,  text_2,  style_2,  text_3,  style_3,    &
                           text_4,  style_4,  text_5,  style_5,  text_6,  style_6,    &
                           text_7,  style_7,  text_8,  style_8,  text_9,  style_9,    &
                           text_10, style_10, text_11, style_11, text_12, style_12,   &
                           text_13, style_13, text_14, style_14, text_15, style_15,   &
                           text_16, style_16, text_17, style_17, text_18, style_18,   &
                           text_19, style_19, text_20, style_20, text_21, style_21,   &
                           text_22, style_22, text_23, style_23, text_24, style_24,   &
                           forward)
    IMPLICIT NONE

    ! TODO: Decrease redundancy using preprocessing directives?
    !       Suggestions welcome!
    CHARACTER (LEN = *), INTENT(IN), OPTIONAL ::   &
                           text_1,  style_1,  text_2,  style_2,  text_3,  style_3,    &
                           text_4,  style_4,  text_5,  style_5,  text_6,  style_6,    &
                           text_7,  style_7,  text_8,  style_8,  text_9,  style_9,    &
                           text_10, style_10, text_11, style_11, text_12, style_12,   &
                           text_13, style_13, text_14, style_14, text_15, style_15,   &
                           text_16, style_16, text_17, style_17, text_18, style_18,   &
                           text_19, style_19, text_20, style_20, text_21, style_21,   &
                           text_22, style_22, text_23, style_23, text_24, style_24,   &
                           forward

    INTEGER         :: output_string_count

    INTEGER         :: output_lengths(512)
    INTEGER         :: i

    CHARACTER * 256 :: format_string
    CHARACTER * 16  :: escape_sequence
    CHARACTER * 256 :: output_strings(512)


    output_string_count = 0
    format_string       = ''


    IF (PRESENT(text_1) .AND. PRESENT(style_1)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_1, escape_sequence)

            ! Append escape sequence to output as single character strings
            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            ! Install formatting for escape sequence
            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        ! Append actual text string to output
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_1
        output_lengths(output_string_count) = LEN(text_1)
    END IF


    IF (PRESENT(text_2) .AND. PRESENT(style_2)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_2, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_2
        output_lengths(output_string_count) = LEN(text_2)
    END IF


    IF (PRESENT(text_3) .AND. PRESENT(style_3)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_3, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_3
        output_lengths(output_string_count) = LEN(text_3)
    END IF


    IF (PRESENT(text_4) .AND. PRESENT(style_4)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_4, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_4
        output_lengths(output_string_count) = LEN(text_4)
    END IF


    IF (PRESENT(text_5) .AND. PRESENT(style_5)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_5, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_5
        output_lengths(output_string_count) = LEN(text_5)
    END IF


    IF (PRESENT(text_6) .AND. PRESENT(style_6)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_6, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_6
        output_lengths(output_string_count) = LEN(text_6)
    END IF


    IF (PRESENT(text_7) .AND. PRESENT(style_7)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_7, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_7
        output_lengths(output_string_count) = LEN(text_7)
    END IF


    IF (PRESENT(text_8) .AND. PRESENT(style_8)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_8, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_8
        output_lengths(output_string_count) = LEN(text_8)
    END IF


    IF (PRESENT(text_9) .AND. PRESENT(style_9)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_9, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_9
        output_lengths(output_string_count) = LEN(text_9)
    END IF


    IF (PRESENT(text_10) .AND. PRESENT(style_10)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_10, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_10
        output_lengths(output_string_count) = LEN(text_10)
    END IF


    IF (PRESENT(text_11) .AND. PRESENT(style_11)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_11, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_11
        output_lengths(output_string_count) = LEN(text_11)
    END IF


    IF (PRESENT(text_12) .AND. PRESENT(style_12)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_12, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_12
        output_lengths(output_string_count) = LEN(text_12)
    END IF


    IF (PRESENT(text_13) .AND. PRESENT(style_13)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_13, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_13
        output_lengths(output_string_count) = LEN(text_13)
    END IF


    IF (PRESENT(text_14) .AND. PRESENT(style_14)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_14, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_14
        output_lengths(output_string_count) = LEN(text_14)
    END IF


    IF (PRESENT(text_15) .AND. PRESENT(style_15)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_15, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_15
        output_lengths(output_string_count) = LEN(text_15)
    END IF


    IF (PRESENT(text_16) .AND. PRESENT(style_16)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_16, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_16
        output_lengths(output_string_count) = LEN(text_16)
    END IF


    IF (PRESENT(text_17) .AND. PRESENT(style_17)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_17, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_17
        output_lengths(output_string_count) = LEN(text_17)
    END IF


    IF (PRESENT(text_18) .AND. PRESENT(style_18)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_18, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_18
        output_lengths(output_string_count) = LEN(text_18)
    END IF


    IF (PRESENT(text_19) .AND. PRESENT(style_19)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_19, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_19
        output_lengths(output_string_count) = LEN(text_19)
    END IF


    IF (PRESENT(text_20) .AND. PRESENT(style_20)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_20, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_20
        output_lengths(output_string_count) = LEN(text_20)
    END IF


    IF (PRESENT(text_21) .AND. PRESENT(style_21)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_21, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_21
        output_lengths(output_string_count) = LEN(text_21)
    END IF


    IF (PRESENT(text_22) .AND. PRESENT(style_22)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_22, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_22
        output_lengths(output_string_count) = LEN(text_22)
    END IF


    IF (PRESENT(text_23) .AND. PRESENT(style_23)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_23, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_23
        output_lengths(output_string_count) = LEN(text_23)
    END IF


    IF (PRESENT(text_24) .AND. PRESENT(style_24)) THEN
        IF (use_escape_codes) THEN
            CALL get_escape_sequence(style_24, escape_sequence)

            DO i = 1, LEN_TRIM(escape_sequence)
                output_string_count = output_string_count + 1
                output_strings(output_string_count) = escape_sequence(i : i)
                output_lengths(output_string_count) = 1
            END DO

            format_string = TRIM(format_string) //   &
                            integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1,A,'
        END IF

        output_string_count = output_string_count + 1
        output_strings(output_string_count) = text_24
        output_lengths(output_string_count) = LEN(text_24)
    END IF


    ! Quit if no arguments were supplied at all
    IF (output_string_count == 0) RETURN


    IF (use_escape_codes) THEN
        ! Install "standard formatting" escape sequence at end of output
        format_string = '(' // TRIM(format_string) // '3A1)'

        output_strings(output_string_count + 1) = CHAR(27)
        output_lengths(output_string_count + 1) = 1
        output_strings(output_string_count + 2) = '['
        output_lengths(output_string_count + 2) = 1
        output_strings(output_string_count + 3) = 'm'
        output_lengths(output_string_count + 3) = 1

        output_string_count = output_string_count + 3

    ELSE
        ! Build format string for output
        format_string = '(' // integer_to_string(output_string_count, 0) // 'A)'
    END IF


    ! Write actual output
    if(present(forward)) then
      WRITE(output_unit, TRIM(format_string), advance=forward)  &
                         (output_strings(i)(1 : output_lengths(i)),   &
                                            i = 1, output_string_count)
    else
      WRITE(output_unit, TRIM(format_string))  &
                         (output_strings(i)(1 : output_lengths(i)),   &
                                            i = 1, output_string_count)
    end if


    RETURN
END SUBROUTINE write_formatted



!------------------------------------------------------------
! SUBROUTINE start_section
!
! Outputs a formatted section start
!------------------------------------------------------------
! Input:
! text:  Label for the section; left-aligned
! style: String describing which styles to set when
!        outputting text (separated by space);
!        see get_escape_sequence for supported styles
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE start_section(text, style)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(IN) :: text, style


    IF (use_escape_codes) THEN
        IF (LEN(text) < section_width) THEN
            ! Fill section title with spaces for uniform section width
            CALL write_formatted(text, style,                              &
                                 REPEAT(' ', section_width - LEN(text)),   &
                                 'bright underline')
        ELSE
            CALL write_formatted(text, style)
        END IF

    ELSE
        WRITE(output_unit, 10) text
        WRITE(output_unit, 10) REPEAT('-', section_width)
    END IF

    WRITE(output_unit, 10) ''


10  FORMAT(A)


    RETURN
END SUBROUTINE start_section



!------------------------------------------------------------
! SUBROUTINE end_section
!
! Outputs a formatted section end
!------------------------------------------------------------
! Input:
! text:  Label for the section; right-aligned
! style: String describing which styles to set when
!        outputting text (separated by space);
!        see get_escape_sequence for supported styles
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE end_section(text, style)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(IN) :: text, style


    IF (use_escape_codes) THEN
        CALL write_formatted(REPEAT(' ', section_width), 'bright underline')
    ELSE
        WRITE(output_unit, 10) ''
        WRITE(output_unit, 10) REPEAT('-', section_width)
    END IF

    IF (LEN(text) < section_width) THEN
        ! Right-align text
        CALL write_formatted(REPEAT(' ', section_width - LEN(text)), 'normal',   &
                             text, style)
    ELSE
        CALL write_formatted(text, style)
    END IF


10  FORMAT(A)


    RETURN
END SUBROUTINE end_section



!------------------------------------------------------------
! SUBROUTINE set_formatting
!
! Sets output formatting to the supplied styles
!------------------------------------------------------------
! Input:
! style_string: String describing which styles to set
!               (separated by space);
!               see get_escape_sequence for supported styles
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE set_formatting(style_string)
    IMPLICIT NONE

    CHARACTER (LEN = *), INTENT(IN) :: style_string

    CHARACTER * 16 :: escape_sequence
    CHARACTER      :: escape_sequence_array(16)
    EQUIVALENCE      (escape_sequence, escape_sequence_array)

    CHARACTER * 16 :: format_string

    INTEGER        :: i


    IF (use_escape_codes) THEN
        CALL get_escape_sequence(TRIM(style_string), escape_sequence)

        format_string = '(' // integer_to_string(LEN_TRIM(escape_sequence), 0) // 'A1)'

        WRITE(output_unit, TRIM(format_string)) (escape_sequence_array(i),   &
                                                 i = 1, LEN_TRIM(escape_sequence))

        ! Move cursor up to compensate for newline inserted
        ! by previous WRITE command
        WRITE(output_unit, '(4A1)') (/ CHAR(27), '[', '2', 'F' /)
    END IF


    RETURN
END SUBROUTINE set_formatting



!------------------------------------------------------------
! SUBROUTINE clear_formatting
!
! Resets output formatting to normal
!------------------------------------------------------------
! Input:
! None
!
! Output:
! None
!------------------------------------------------------------
SUBROUTINE clear_formatting()
    IMPLICIT NONE


    IF (use_escape_codes) THEN
        ! Clear all previously set styles
        WRITE(output_unit, '(3A1)') (/ CHAR(27), '[', 'm' /)

        ! Move cursor up to compensate for newline inserted
        ! by previous WRITE command
        WRITE(output_unit, '(4A1)') (/ CHAR(27), '[', '2', 'F' /)
    END IF


    RETURN
END SUBROUTINE clear_formatting



END MODULE Foul_Mod

