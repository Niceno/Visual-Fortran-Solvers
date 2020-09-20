!==============================================================================!
  subroutine Get_Escape_Sequence(style_string, escape_sequence)
!------------------------------------------------------------------------------!
!   Helper function
!   Generates an ANSI escape sequence from the supplied style string
!------------------------------------------------------------------------------!
!   Input:
!   style_string: String describing which styles to set (separated by space);
!                 see source code for supported styles
!
!   Output:
!   escape_sequence: ANSI escape sequence generated from the specified styles
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(*),  intent(in)  :: style_string
  character(16), intent(out) :: escape_sequence
!------------------------------------------------------------------------------!
  integer       :: i
  character(32) :: style_substrings(16)
  integer       :: style_substring_count
!==============================================================================!

  ! Start sequence with command to clear any previous attributes
  escape_sequence = char(27) // '[0'

  call Split_String(trim(style_string), ' ', style_substrings, style_substring_count)

  do i = 1, style_substring_count
    call Lower_Case(style_substrings(i))

    select case (trim(style_substrings(i)))
      case ('bright')
          escape_sequence = trim(escape_sequence) // ';1'
      case ('faint')
          escape_sequence = trim(escape_sequence) // ';2'
      case ('italic')
          escape_sequence = trim(escape_sequence) // ';3'
      case ('underline')
          escape_sequence = trim(escape_sequence) // ';4'
      case ('blink_slow')
          escape_sequence = trim(escape_sequence) // ';5'
      case ('blink_fast')
          escape_sequence = trim(escape_sequence) // ';6'
      case ('black')
          escape_sequence = trim(escape_sequence) // ';30'
      case ('red')
          escape_sequence = trim(escape_sequence) // ';31'
      case ('green')
          escape_sequence = trim(escape_sequence) // ';32'
      case ('yellow')
          escape_sequence = trim(escape_sequence) // ';33'
      case ('blue')
          escape_sequence = trim(escape_sequence) // ';34'
      case ('magenta')
          escape_sequence = trim(escape_sequence) // ';35'
      case ('cyan')
          escape_sequence = trim(escape_sequence) // ';36'
      case ('white')
          escape_sequence = trim(escape_sequence) // ';37'
      case ('background_black')
          escape_sequence = trim(escape_sequence) // ';40'
      case ('background_red')
          escape_sequence = trim(escape_sequence) // ';41'
      case ('background_green')
          escape_sequence = trim(escape_sequence) // ';42'
      case ('background_yellow')
          escape_sequence = trim(escape_sequence) // ';43'
      case ('background_blue')
          escape_sequence = trim(escape_sequence) // ';44'
      case ('background_magenta')
          escape_sequence = trim(escape_sequence) // ';45'
      case ('background_cyan')
          escape_sequence = trim(escape_sequence) // ';46'
      case ('background_white')
          escape_sequence = trim(escape_sequence) // ';47'
    end select
  end do

  ! Append end of sequence marker
  escape_sequence = trim(escape_sequence) // 'm'

  end subroutine

