!==============================================================================!
  subroutine Foul_Mod_Write(                                                  &
                   text_1,  style_1,  text_2,  style_2,  text_3,  style_3,    &
                   text_4,  style_4,  text_5,  style_5,  text_6,  style_6,    &
                   text_7,  style_7,  text_8,  style_8,  text_9,  style_9,    &
                   text_10, style_10, text_11, style_11, text_12, style_12,   &
                   text_13, style_13, text_14, style_14, text_15, style_15,   &
                   text_16, style_16, text_17, style_17, text_18, style_18,   &
                   text_19, style_19, text_20, style_20, text_21, style_21,   &
                   text_22, style_22, text_23, style_23, text_24, style_24,   &
                   forward)
!------------------------------------------------------------------------------!
!   Outputs the supplied strings on a single line,
!   formatted using the supplied styles
!------------------------------------------------------------------------------!
!   Input:
!   text_1:  First string to output
!   style_1: String describing which styles to set when
!            outputting text_1 (separated by space);
!            see Get_Escape_Sequence for supported styles
!   [...] (up to 24 text strings supported)
!
!   Output:
!   None
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(*), intent(in), optional ::                                       &
                   text_1,  style_1,  text_2,  style_2,  text_3,  style_3,    &
                   text_4,  style_4,  text_5,  style_5,  text_6,  style_6,    &
                   text_7,  style_7,  text_8,  style_8,  text_9,  style_9,    &
                   text_10, style_10, text_11, style_11, text_12, style_12,   &
                   text_13, style_13, text_14, style_14, text_15, style_15,   &
                   text_16, style_16, text_17, style_17, text_18, style_18,   &
                   text_19, style_19, text_20, style_20, text_21, style_21,   &
                   text_22, style_22, text_23, style_23, text_24, style_24,   &
                   forward
!------------------------------------------------------------------------------!
  integer        :: output_string_count
  integer        :: output_lengths(512)
  integer        :: i
  character(256) :: format_string
  character (16) :: escape_sequence
  character(256) :: output_strings(512)
!==============================================================================!

  output_string_count = 0
  format_string       = ''

  if (present(text_1) .and. present(style_1)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_1, escape_sequence)

      ! Append escape sequence to output as single character strings
      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      ! Install formatting for escape sequence
      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    ! Append actual text string to output
    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_1
    output_lengths(output_string_count) = len(text_1)
  end if

  if (present(text_2) .and. present(style_2)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_2, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_2
    output_lengths(output_string_count) = len(text_2)
  end if

  if (present(text_3) .and. present(style_3)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_3, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_3
    output_lengths(output_string_count) = len(text_3)
  end if

  if (present(text_4) .and. present(style_4)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_4, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_4
    output_lengths(output_string_count) = len(text_4)
  end if

  if (present(text_5) .and. present(style_5)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_5, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_5
    output_lengths(output_string_count) = len(text_5)
  end if

  if (present(text_6) .and. present(style_6)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_6, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_6
    output_lengths(output_string_count) = len(text_6)
  end if

  if (present(text_7) .and. present(style_7)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_7, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_7
    output_lengths(output_string_count) = len(text_7)
  end if

  if (present(text_8) .and. present(style_8)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_8, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_8
    output_lengths(output_string_count) = len(text_8)
  end if

  if (present(text_9) .and. present(style_9)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_9, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_9
    output_lengths(output_string_count) = len(text_9)
  end if

  if (present(text_10) .and. present(style_10)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_10, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_10
    output_lengths(output_string_count) = len(text_10)
  end if

  if (present(text_11) .and. present(style_11)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_11, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_11
    output_lengths(output_string_count) = len(text_11)
  end if

  if (present(text_12) .and. present(style_12)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_12, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_12
    output_lengths(output_string_count) = len(text_12)
  end if

  if (present(text_13) .and. present(style_13)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_13, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_13
    output_lengths(output_string_count) = len(text_13)
  end if

  if (present(text_14) .and. present(style_14)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_14, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_14
    output_lengths(output_string_count) = len(text_14)
  end if

  if (present(text_15) .and. present(style_15)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_15, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_15
    output_lengths(output_string_count) = len(text_15)
  end if

  if (present(text_16) .and. present(style_16)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_16, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_16
    output_lengths(output_string_count) = len(text_16)
  end if

  if (present(text_17) .and. present(style_17)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_17, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_17
    output_lengths(output_string_count) = len(text_17)
  end if

  if (present(text_18) .and. present(style_18)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_18, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_18
    output_lengths(output_string_count) = len(text_18)
  end if

  if (present(text_19) .and. present(style_19)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_19, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_19
    output_lengths(output_string_count) = len(text_19)
  end if

  if (present(text_20) .and. present(style_20)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_20, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_20
    output_lengths(output_string_count) = len(text_20)
  end if

  if (present(text_21) .and. present(style_21)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_21, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_21
    output_lengths(output_string_count) = len(text_21)
  end if

  if (present(text_22) .and. present(style_22)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_22, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_22
    output_lengths(output_string_count) = len(text_22)
  end if

  if (present(text_23) .and. present(style_23)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_23, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_23
    output_lengths(output_string_count) = len(text_23)
  end if

  if (present(text_24) .and. present(style_24)) then
    if (use_escape_codes) then
      call Get_Escape_Sequence(style_24, escape_sequence)

      do i = 1, len_trim(escape_sequence)
        output_string_count = output_string_count + 1
        output_strings(output_string_count) = escape_sequence(i : i)
        output_lengths(output_string_count) = 1
      end do

      format_string = trim(format_string) //   &
                      Integer_To_String(len_trim(escape_sequence), 0) // 'A1,A,'
    end if

    output_string_count = output_string_count + 1
    output_strings(output_string_count) = text_24
    output_lengths(output_string_count) = len(text_24)
  end if

    ! Quit if no arguments were supplied at all
  if (output_string_count == 0) return

  if (use_escape_codes) then
    ! Install "standard formatting" escape sequence at end of output
    format_string = '(' // trim(format_string) // '3A1)'

    output_strings(output_string_count + 1) = char(27)
    output_lengths(output_string_count + 1) = 1
    output_strings(output_string_count + 2) = '['
    output_lengths(output_string_count + 2) = 1
    output_strings(output_string_count + 3) = 'm'
    output_lengths(output_string_count + 3) = 1

    output_string_count = output_string_count + 3

  else
    ! Build format string for output
    format_string = '(' // Integer_To_String(output_string_count, 0) // 'A)'
  end if

  ! Write actual output
  if(present(forward)) then
    write(output_unit, trim(format_string), advance=forward)  &
                       (output_strings(i)(1 : output_lengths(i)),   &
                                          i = 1, output_string_count)
  else
    write(output_unit, trim(format_string))  &
                       (output_strings(i)(1 : output_lengths(i)),   &
                                          i = 1, output_string_count)
  end if

  end subroutine

