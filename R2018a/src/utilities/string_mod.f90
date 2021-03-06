!
! Copyright (C) 2016 - 2017 Michael A. Maedo
!
module string_mod
  
    use precision_mod, only: BYTE, LONG, double

    use parameters_mod, only: ZERO

    use error_mod, only: &
        error, &
        & NO_ERROR, &
        & ERROR_INVALID_CARD,  &
        & ERROR_WORD_NOT_FOUND,  &
        & ERROR_SIZE_STRING_OF_NUM, &
        & ERROR_SIZE_STRING_OF_TXT,  & 
        & INFO_SIZE_STRING

#include "error.fpp"
    
    implicit none
    
    integer(LONG), parameter :: LEN_VAR_NAME  = 5
    integer(LONG), parameter :: LEN_NAME_FILE = 100
    integer(LONG), parameter :: LEN_STR       = 100
    integer(LONG), parameter :: MAX_NWORDS    = 200
    integer(LONG), parameter :: MAX_LEN       = 800
    
    intrinsic char, achar, iachar, len_trim, len, trim, &
        new_line, size, scan, present
    
contains

    
    !Search 'word' in txt, and store the quantity in res
    function get_real(word, txt, num, err) result (res)

        real(double), intent(in) :: num(:) !numerical values associated with txt 
        character(len=*), intent(in) :: txt(:) !array of strings to be scanned to find 'word' 
        character(len = *), intent(in)  :: word !word to be searched in txt 
        type(error), intent(inout) :: err !if (err % status == ERROR_WORD_NOT_FOUND) means that 'word' was not found in txt; otherwise is ok
        real(double) :: res !quantity associated with the input 'word' 

        integer(LONG) :: iword, nwords
        character(len = LEN_STR) :: upper_word
        character(len = LEN_STR) :: upper_txt
!
!       Convert characters of word to uppercase if it is need
        upper_word = uppercase(word)
!
!       Variable initialization
        res = 0; nwords = size(txt)
!
!       Search the quantity of word in the array txt
        do iword = 1, nwords
!
!           Check if the sizes of upper_word and txt(iword) match
            if ( len_trim(upper_word) == len_trim(txt(iword)) ) then
!
!               Convert characters of txt to uppercase
                upper_txt = uppercase(trim(txt(iword)))
            end if
!
!           Exit do-loop if the quantity of word was found in txt
            if (upper_txt == upper_word) then
                res  = num(iword)
                return
            end if
        end do
        RAISE( ERROR_WORD_NOT_FOUND, err )
        
    end function get_real


    !Search 'word' in txt, and store the quantity in res
    function get_int(word, txt, num, err) result (res)

        integer(LONG)     , intent(in) :: num(:) !numerical values associated with txt
        character(len = *), intent(in) :: word !word to be found in txt
        character(len = *), intent(in) :: txt(:) !array of strings to be scanned to find 'word'
        type(error), intent(inout) :: err !f (error == ERROR_WORD_NOT_FOUND) means that 'word' was not found in txt, otherwise is ok
        integer(LONG) :: res !quantity associated with the input 'word'

        integer(LONG) :: iword, nwords
        character(len = len_trim(word)) :: upper_word
        character(len = len_trim(word)) :: upper_txt
!
!       Convert characters of word to uppercase if it is need
        upper_word = uppercase(trim(word))
!
!       Variable initialization
        res = 0; nwords = size(txt)
!
!       Search the quantity of word in the array txt
        do iword = 1, nwords
!
!           Check if the sizes of upper_word and txt(iword) match
            if ( len_trim(upper_word) == len_trim(txt(iword)) ) then
!
!               Convert characters of txt to uppercase
                upper_txt = uppercase(trim(txt(iword)))
            end if
!
!           Exit do-loop if the quantity of word was found in txt
            if (upper_txt == upper_word) then
                res  = num(iword)
                return
            end if
        end do
        RAISE( ERROR_WORD_NOT_FOUND, err )

    end function get_int

    
    !Convert characters to uppercase
    function uppercase(in_str) result (out_str)

        character(len = *), intent(in) :: in_str !string to be converted to uppercase
        character(len = len(in_str)) :: out_str !contain 'in_str' with uppercase characters

        integer(LONG) :: i, j

        do i = 1, len(in_str)
            j = iachar(in_str(i:i))
            if (j >= iachar("a") .and. j <= iachar("z") ) then
                out_str(i:i) = achar(iachar(in_str(i:i)) - 32)
            else
                out_str(i:i) = in_str(i:i)
            end if
        end do
    end function uppercase


    !Compare two strings ans return .true. if the two are equal
    function strcmp(card, fcard) result (ans)

        character(len = *), intent(in) :: card !first string to be compared
        character(len = *), intent(in) :: fcard !second string to be compared
        integer(LONG) :: ans !is .true. if card and fcard are identical, otherwise is .false.

        integer(LONG)                    :: error
        character(len = len_trim(card))  :: upper_card
        character(len = len_trim(fcard)) :: upper_fcard

        upper_fcard = uppercase(trim(fcard))
        upper_card  = uppercase(trim(card))

        ans = ERROR_INVALID_CARD
        if (upper_card == upper_fcard) ans = NO_ERROR
    end function strcmp


    !Split the string stored in str into texts and numerical values
    subroutine split_txt_num(str, nwords, begin, end_word, txt, ntxt, num, nnum, err)

        integer(LONG)     , intent(in)    :: nwords !number of words in str
        integer(LONG)     , intent(in)    :: begin(:) !positions of the 1st char of each word in str
        integer(LONG)     , intent(in)    :: end_word(:) !positions of the last char of each word in str
        character(len = *), intent(in)    :: str !alphanumeric string to be splitted

        integer(LONG)     , intent(inout) :: ntxt !number of strings
        integer(LONG)     , intent(inout) :: nnum !number of numerical values
        character(len = *), intent(inout) :: txt(:) !array of texts. It is obtained from str
        real(DOUBLE)      , intent(inout) :: num(:) !arrays of numerical values. It is obtained from str
        type(error), intent(inout) :: err !(err % status == ERROR_SIZE_STRING) means that limit of the array num or txt has been exceeded, otherwise is ok

        real(double)  :: loc_num
        integer(LONG) :: info
        integer(LONG) :: iword, is_num
        integer(LONG) :: len_word
        integer(LONG) :: ii, jj

        info = NO_ERROR; ii = 0; jj = 0
        do iword = 1, nwords
            ii = begin(iword)
            jj = end_word(iword)
!
!           Length of ith-word in str
            len_word = jj - ii + 1
            read(str(ii:jj), fmt = '(F25.0)', iostat = is_num) loc_num
!
!           If it is a number store it in num
            if (is_num == 0) then
                nnum = nnum + 1
!
!               Check if nnum exceeds the dimension of num
                if (nnum > size(num)) then
                    RAISE( ERROR_SIZE_STRING_OF_NUM, err )
                end if
                num(nnum) = loc_num
!
!           If it is a word (starting with a letter) store it in txt
            else
                ntxt = ntxt + 1
!
!               Check if ntxt exceeds the dimension of txt
                if (ntxt > size(txt)) then
                    RAISE( ERROR_SIZE_STRING_OF_TXT, err )
                end if
                
                txt(ntxt) = str(ii:jj)
             end if
        end do

    end subroutine split_txt_num

end module string_mod
