c
c-----------------------------------------------------------------------
c
c ****** Macro expander.
c
c-----------------------------------------------------------------------
c
c ****** Updates and bug fixes:
c
c        02/10/2000, ZM, Version 1.00:
c
c         - Original version of program.
c           Added a command-line interface to the original version.
c
c        07/29/2003, ZM, Version 1.01:
c
c         - This version uses the ZM tools libraries.
c
c        04/25/2012, ZM, Version 1.02:
c
c         - Added the option of specifying macro definitions in a
c           separate file from the source file.  This is useful when
c           a code is split into individual source files for each
c           routine.
c         - Improved the formatting of the debugging information.
c         - Performed a slight cleanup of the code.
c
c        08/16/2012, ZM, Version 1.03:
c
c         - Fixed some minor bugs that were discovered by compiling
c           with GFORTRAN.
c
c        04/28/2021, RC, Version 1.03r:
c
c         - Manually added parse and number type module code
c           so this tool is a stand-alone release.
c
c        04/028/2025, RC, Version 2.0.0:
c
c         - Updated code to work for Fortran 90 continuation and
c           comments syntax.
c           This is for GIT version of MAS.  It will not work for the
c           old versions of MAS.
c
c#######################################################################
      module number_types
c
      use iso_fortran_env
c
      implicit none
c
c
c ****** Set up the KIND values for the various REALs.
c ****** These can be compiler dependent.
c
      integer, parameter :: KIND_REAL_4=REAL32
      integer, parameter :: KIND_REAL_8=REAL64
c
c ****** KIND values for specifying the precision of REALs.
c
      integer, private, parameter :: r4=KIND_REAL_4
      integer, private, parameter :: r8=KIND_REAL_8
c
c ****** Select the number type for REALs (one of: R4|R8).
c
      integer, parameter :: r_typ=r8
c
      end module
c#######################################################################
      module syntax
c
c-----------------------------------------------------------------------
c ****** Group definitions for parsing command-line arguments.
c-----------------------------------------------------------------------
c
c        GROUP 1: <kw>
c        GROUP 2: <arg>
c        GROUP 3: <kw> <arg>
c        GROUP 4: <kw> <arg> <arg>
c
      integer, parameter :: ngroups=4
c
      integer, parameter :: GROUP_K  =1
      integer, parameter :: GROUP_A  =2
      integer, parameter :: GROUP_KA =3
      integer, parameter :: GROUP_KAA=4
c
      end module
c#######################################################################
      module string_def
c
c-----------------------------------------------------------------------
c ****** Define a structure to hold a string.
c-----------------------------------------------------------------------
c
      implicit none
c
      type :: string
        character, dimension(:), pointer :: c
      end type
c
      end module
c#######################################################################
      module paragraph_def
c
      use string_def
c
      implicit none
c
c-----------------------------------------------------------------------
c ****** Define a structure for a linked list of lines
c ****** (i.e., a paragraph).
c-----------------------------------------------------------------------
c
      type :: paragraph
        type(string) :: line
        type(paragraph), pointer :: next
      end type
c
c-----------------------------------------------------------------------
c ****** Define a structure to hold a list of paragraphs.
c-----------------------------------------------------------------------
c
      type :: parlist
        type(paragraph), pointer :: par
      end type
c
      end module
c#######################################################################
      module lcase_interface
      interface
        function lcase (s)
        character(*), intent(in) :: s
        character(len(s)) :: lcase
        end
      end interface
      end module
c#######################################################################
      module ucase_interface
      interface
        function ucase (s)
        character(*), intent(in) :: s
        character(len(s)) :: ucase
        end
      end interface
      end module
c#######################################################################
      module new_par_interface
      interface
        subroutine new_par (par)
        use paragraph_def
        implicit none
        type(paragraph), pointer :: par
        end
      end interface
      end module
c#######################################################################
      module delete_par_interface
      interface
        subroutine delete_par (par)
        use paragraph_def
        implicit none
        type(paragraph), pointer :: par
        end
      end interface
      end module
c#######################################################################
      module add_line_interface
      interface
        subroutine add_line (line,par)
        use paragraph_def
        implicit none
        character(*) :: line
        type(paragraph), pointer :: par
        end
      end interface
      end module
c#######################################################################
      module print_par_interface
      interface
        subroutine print_par (par)
        use paragraph_def
        implicit none
        type(paragraph), pointer :: par
        end
      end interface
      end module
c#######################################################################
      module get_str_interface
      interface
        function get_str (str)
        use string_def
        implicit none
        type(string) :: str
        character(size(str%c)) :: get_str
        end
      end interface
      end module
c#######################################################################
      module get_usage_line_interface
      interface
        subroutine get_usage_line (usage)
        use paragraph_def
        implicit none
        type(paragraph), pointer :: usage
        end
      end interface
      end module
c#######################################################################
      module params
c
c-----------------------------------------------------------------------
c ****** Parameters.
c-----------------------------------------------------------------------
c
      implicit none
c
c ****** File names.
c
      character(512) :: infile
      character(512) :: outfile
      character(512) :: macro_def_file
c
c ****** Debug level.
c
      integer :: idebug
c
      end module
c#######################################################################
      module get_statement_vars
c
      implicit none
c
c-----------------------------------------------------------------------
c ****** Define the maximum number of characters allowed in a line.
c-----------------------------------------------------------------------
c
      integer, parameter :: LMX=132
c
c ****** Input line buffer.
c
      character*(LMX) :: line
c
c-----------------------------------------------------------------------
c ****** GETSTMT state variables.
c-----------------------------------------------------------------------
c
c ****** Current line number.
c
      integer :: ln
c
c ****** Switch to continue reading a statement.
c
      logical :: rdfil
c
c-----------------------------------------------------------------------
c
      end module
c#######################################################################
      module macro_defs
c
c-----------------------------------------------------------------------
c ****** Macro definition tables.
c-----------------------------------------------------------------------
c
      implicit none
c
      integer, parameter :: n_macros_max=100
      integer, parameter :: LMNAM=64
      integer, parameter :: LMRPL=512
      integer, parameter :: LMARG=64
c
      integer :: nmac=0
      character(LMNAM), dimension(n_macros_max) :: macnam
      character(LMRPL), dimension(n_macros_max) :: macrpl
      integer, dimension(n_macros_max) :: macnarg
c
      end module
c#######################################################################
      module buffers
c
c-----------------------------------------------------------------------
c ****** Input and output line buffers.
c-----------------------------------------------------------------------
c
      implicit none
c
      integer, parameter :: LBUF=20480
      character(LBUF) :: ibuf,obuf
c
      end module
c#######################################################################
      program EXPMAC
c
c-----------------------------------------------------------------------
c
      use params
      use macro_defs
      use buffers
c
c-----------------------------------------------------------------------
c
      character(*), parameter :: cname='EXPMAC'
      character(*), parameter :: cvers='2.0.0'
      character(*), parameter :: cdate='04/02/2025'
c
c-----------------------------------------------------------------------
c
      logical :: mdef,mres
      integer :: ierr
      character(512) :: line
c
c-----------------------------------------------------------------------
c
      logical, external :: macdef,macres
c
c-----------------------------------------------------------------------
c
c ****** Set the parameters.
c
      call set_parameters (cname,cvers,cdate)
c
c ****** Check that the input and output file names are not identical.
c
      if (infile.eq.outfile) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Input and output file names are identical.'
        call exit (1)
      end if
c
c ****** If a separate macro definition file was specified,
c ****** parse the macros contained in it.
c
      if (macro_def_file.ne.' ') then
        call read_macro_file (macro_def_file)
      end if
c
c ****** Open the input and output files.
c
      call ffopen (1,infile,'r',ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not open the input file.'
        write (*,*) 'File name: ',trim(infile)
        call exit (1)
      end if
c
      call ffopen (2,outfile,'rw',ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not open the output file.'
        write (*,*) 'File name: ',trim(outfile)
        call exit (1)
      end if
c
c ****** Initialization.
c
      ls=0
      call getstmt (0,0,ibuf,ixi,ixfc,ierr)
c
c ****** Main loop.
c
  100 continue
c
c ****** Get the next statement.
c
      call getstmt (1,2,ibuf,ixi,ixfc,ierr)
      if (ierr.ne.0) go to 900
c
      ls=ls+1
c
      if (idebug.ge.3) then
        write (*,*)
        write (*,*) '### Input file: read a statement:'
        write (*,*) 'Statement #',ls,':'
        write (*,*) ibuf(1:ixi-1)
      end if
c
c ****** Check the line for macro calls.
c
      ixo=1
      mres=macres(ibuf(1:ixi-1),obuf,ixo)
      if (mres) then
        if (idebug.ge.2) then
          write (*,*)
          write (*,*) '### Input file: macro call:'
          write (*,*) 'Statement #',ls,':'
          write (*,*) 'Replacement text:'
          write (*,*) obuf(1:ixo-1)
        end if
      end if
c
c ****** Check if it is a macro definition.
c
      mdef=macdef(obuf(1:ixo-1))
      if (mdef) then
c
        if (idebug.ge.1) then
          write (*,*)
          write (*,*) '### Input file: macro definition:'
          write (*,*) 'Statement #',ls,':'
          write (*,*) 'Macro number = ',nmac
          write (*,*) 'Macro name = ',macnam(nmac)
          write (*,*) 'Number of arguments = ',macnarg(nmac)
          write (*,*) 'Replacement text:'
          lrpl=lenstr(macrpl(nmac))
          write (*,*) macrpl(nmac)(1:lrpl)
        end if
c
      else
c
c ****** Write the line to the output file.
c
        line=' '
        ix=ixfc-1

        do i=1,ixo-1
          ix=ix+1
          line(ix:ix)=obuf(i:i)
          if (ix.gt.72.and.(ixo-1+ixfc-1).gt.73) then
            write (2,'(a)') line(1:ix)//'&'
            ix=1
            line=' '
            line(1:1)='&'
          end if
        enddo
        if (ix.gt.0) then
          if (line(1:ix).ne.'&') write (2,'(a)') line(1:ix)
        end if
c
      end if
c
      go to 100
c
  900 continue
c
      close (1)
      close (2)
c
      call exit (0)
c
      end
c#######################################################################
      subroutine read_macro_file (fname)
c
c-----------------------------------------------------------------------
c
c ****** Read macro definitions from file FNAME.
c
c-----------------------------------------------------------------------
c
      use params
      use get_statement_vars
      use macro_defs
      use buffers
c
c-----------------------------------------------------------------------
c
      character(*) :: fname
c
c-----------------------------------------------------------------------
c
      integer :: ierr
      integer :: ixi,ixfc,ixo,ls
      logical :: mres,mdef
c
c-----------------------------------------------------------------------
c
      logical, external :: macdef,macres
c
c-----------------------------------------------------------------------
c
c ****** Open the macro definition file.
c
      call ffopen (3,fname,'r',ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in READ_MACRO_FILE:'
        write (*,*) '### Could not open the macro definition file:'
        write (*,*) 'File name: ',trim(fname)
        call exit (1)
      end if
c
      if (idebug.ge.1) then
        write (*,*)
        write (*,*) '### Reading macro definition file: ',trim(fname)
      end if
c
c ****** Initialization.
c
      ls=0
      call getstmt (0,0,ibuf,ixi,ixfc,ierr)
c
c ****** Main loop.
c
  100 continue
c
c ****** Get the next statement.
c
      call getstmt (3,0,ibuf,ixi,ixfc,ierr)
      if (ierr.ne.0) go to 900
c
      ls=ls+1
c
      if (idebug.ge.3) then
        write (*,*)
        write (*,*) '### Macro definition file: read a statement:'
        write (*,*) 'Statement #',ls,':'
        write (*,*) ibuf(1:ixi-1)
      end if
c
c ****** Check the line for macro calls.
c
      ixo=1
      mres=macres(ibuf(1:ixi-1),obuf,ixo)
      if (mres) then
        if (idebug.ge.2) then
          write (*,*)
          write (*,*) '### Macro definition file: macro call:'
          write (*,*) 'Statement #',ls,':'
          write (*,*) 'Replacement text:'
          write (*,*) obuf(1:ixo-1)
        end if
      end if
c
c ****** Check if it is a macro definition.
c
      mdef=macdef(obuf(1:ixo-1))
      if (mdef) then
c
        if (idebug.ge.1) then
          write (*,*)
          write (*,*) '### Macro definition file: macro definition:'
          write (*,*) 'Statement #',ls,':'
          write (*,*) 'Macro number = ',nmac
          write (*,*) 'Macro name = ',macnam(nmac)
          write (*,*) 'Number of arguments = ',macnarg(nmac)
          write (*,*) 'Replacement text:'
          lrpl=lenstr(macrpl(nmac))
          write (*,*) macrpl(nmac)(1:lrpl)
        end if
c
      else
c
c ****** It is a fatal error to have statments in the macro
c ****** definition file that do not define a macro.
c
        write (*,*)
        write (*,*) '### ERROR in READ_MACRO_FILE:'
        write (*,*) '### Encountered a statement that does not'//
     &              ' define a macro:'
        write (*,*) obuf(1:ixo-1)
        call exit (1)
c
      end if
c
      go to 100
c
  900 continue
c
      close (3)
c
      return
      end
c#######################################################################
      logical function getname (line,ix,name,isl,delim)
c
c-----------------------------------------------------------------------
c
      character*(*) line,name
      logical delim
c
c-----------------------------------------------------------------------
c
      logical letter,alpha
c
c-----------------------------------------------------------------------
c
      getname=.false.
      delim=.false.
      name=' '
c
      lline=lenstr(line)
      lname=len(name)
c
c ****** Find the next name.
c
  100 continue
      if (ix.gt.lline) return
      if (.not.letter(line(ix:ix))) then
        if (isl.gt.0.and.line(ix:ix).eq.']') then
          delim=.true.
          return
        end if
        ix=ix+1
        go to 100
      end if
c
c ****** Get the name.
c
      ln=1
      name(1:1)=line(ix:ix)
      ix=ix+1
      getname=.true.
  200 continue
      if (ix.gt.lline) return
      if (alpha(line(ix:ix))) then
        ln=ln+1
        if (ln.gt.lname) then
          write (*,*)
          write (*,*) '### ERROR in GETNAME:'
          write (*,*) '### A variable name exceeds the maximum'//
     &                ' number of characters allowed.'
          write (*,*) 'Number of characters allowed = ',lname
          write (*,*) 'Truncated variable name: ',name
          call exit (1)
        end if
        name(ln:ln)=line(ix:ix)
        ix=ix+1
        go to 200
      end if
c
      return
      end
c#######################################################################
      logical function alpha (ch)
c
c-----------------------------------------------------------------------
c
      character ch
c
c-----------------------------------------------------------------------
c
      alpha=.false.
c
      ich=ichar(ch)
c
      if ((ich.ge.65.and.ich.le. 90).or.
     &    (ich.ge.97.and.ich.le.122).or.
     &    (ich.ge.48.and.ich.le. 57).or.
     &    ich.eq.95) alpha=.true.
c
      return
      end
c#######################################################################
      logical function letter (ch)
c
c-----------------------------------------------------------------------
c
      character ch
c
c-----------------------------------------------------------------------
c
      letter=.false.
c
      ich=ichar(ch)
c
      if ((ich.ge.65.and.ich.le. 90).or.
     &    (ich.ge.97.and.ich.le.122)) letter=.true.
c
      return
      end
c#######################################################################
      function lenstr (str)
c
c-----------------------------------------------------------------------
c
c ****** Get the length of string STR, ignoring right-filling blanks.
c
c-----------------------------------------------------------------------
c
      character*(*) str
c
c-----------------------------------------------------------------------
c
      do i=len(str),1,-1
        if (str(i:i).ne.' ') go to 100
      enddo
      i=0
  100 continue
c
      lenstr=i
c
      return
      end
c#######################################################################
      function match (table,n,alpha)
c
      character*(*) table(n),alpha
c
      do i=1,n
        if (table(i).eq.alpha) then
          match=i
          return
        end if
      enddo
c
      match=0
c
      return
      end
c#######################################################################
      logical function find (line,ch,ix)
c
c-----------------------------------------------------------------------
c
c ****** Find the index of the first occurence of
c ****** character CH in LINE, starting from position IX.
c
c ****** If found, set FIND=.true., and set IX to be its index.
c
c-----------------------------------------------------------------------
c
      character*(*) line
      character ch
c
c-----------------------------------------------------------------------
c
      find=.false.
c
      ll=lenstr(line)
c
      ix0=ix
c
      do 100 i=ix0,ll
        if (line(i:i).eq.ch) then
          ix=i
          find=.true.
          return
        end if
  100 continue
c
      return
      end
c#######################################################################
      function ifind (line,ch1,ch2,ix)
c
c-----------------------------------------------------------------------
c
c ****** Find the index of the first occurence of
c ****** character CH1 or CH2 in LINE, starting from
c ****** position IX.
c
c ****** Set IFIND=1 if CH1 was found, or set IFIND=2 if
c ****** CH2 was found, and set IX to be its index.
c ****** Set IFIND=0 if neither character was found.
c
c-----------------------------------------------------------------------
c
      character*(*) line
      character ch1,ch2
c
c-----------------------------------------------------------------------
c
      ifind=0
c
      ll=lenstr(line)
c
      ix0=ix
c
      do 100 i=ix0,ll
        if (line(i:i).eq.ch1) then
          ix=i
          ifind=1
          return
        else if (line(i:i).eq.ch2) then
          ix=i
          ifind=2
          return
        end if
  100 continue
c
      return
      end
c#######################################################################
      subroutine addtobuf (line,buf,ix,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Add the string in LINE to the buffer BUF, starting
c ****** at character position IX in BUF.
c
c-----------------------------------------------------------------------
c
c ****** When COMPRESS=.true., multiple spaces in LINE are
c ****** converted into a single space.
c
c-----------------------------------------------------------------------
c
      logical COMPRESS
      parameter (COMPRESS=.false.)
c
c-----------------------------------------------------------------------
c
      character*(*) line,buf
c
c-----------------------------------------------------------------------
c
      logical psp
c
c-----------------------------------------------------------------------
c
      ierr=0
c
      lbuf=len(buf)
      ll=len(line)
c
      psp=.false.
      do 100 i=1,ll
c
        if (COMPRESS) then
c
c ****** Compress multiple spaces into a single space.
c
          if (line(i:i).eq.' ') then
            if (psp) go to 100
            psp=.true.
          else
            psp=.false.
          end if
        end if
c
c ****** Check for a buffer overflow.
c
        if (ix.gt.lbuf) then
          ierr=1
          return
        end if
c
c ****** Write the character to the buffer.
c
        buf(ix:ix)=line(i:i)
        ix=ix+1
c
  100 continue
c
      return
      end
c#######################################################################
      integer function fchar (str)
c
c-----------------------------------------------------------------------
c
c ****** Find the position of the first non-blank character in STR.
c
c-----------------------------------------------------------------------
c
      character*(*) str
c
c-----------------------------------------------------------------------
c
      do i=1,len(str)
        if (str(i:i).ne.' ') go to 100
      enddo
      i=0
  100 continue
c
      fchar=i
c
      return
      end
c#######################################################################
      subroutine getstmt (iun,iuno,buf,ix,ixfc,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Return the next whole FORTRAN statement from the
c ****** file connected to unit IUN.
c
c-----------------------------------------------------------------------
c
c ****** The statement is returned in BUF.  IX-1 is the index
c ****** of the last character filled in BUF.
c
c ****** If a statement is returned, IERR=0 is set.
c ****** IERR=1 is returned when no more statements can be
c ****** found in the file.
c
c ****** IXFC is the position of the first non-blank character
c ****** in the statement.  BUF is filled with the statement
c ****** without including any leading spaces.
c ****** Multiple spaces are combined into a single space.
c
c ****** This routine skips blank lines.
c ****** If a statement is a comment, it is written
c ****** (without any processing) to unit IUNO if IUNO>0.
c ****** Set IUNO=0 to exclude comments from the output file.
c
c ****** This routine must be initialized with IUN=0
c ****** before it is used to return the first statement.
c
c-----------------------------------------------------------------------
c
      use get_statement_vars
c
c-----------------------------------------------------------------------
c
      character(*) :: buf
c
c-----------------------------------------------------------------------
c
c ****** Backslash character ("\").
c
      character(1), parameter :: bslash=achar(92)
c
c-----------------------------------------------------------------------
c
      logical :: defcont
c
c-----------------------------------------------------------------------
c
      integer, external :: fchar
      logical, external :: rdlin
c
c-----------------------------------------------------------------------
c
      ierr=0
c
c ****** Check if this is an initialization.
c
      if (iun.eq.0) then
        rdfil=.true.
        ln=0
        return
      end if
c
      lbuf=len(buf)
      defcont=.false.
      ix=1
c
  100 continue
c
c ****** Get the next line.
c
      if (rdfil) then
        if (.not.rdlin(iun,line)) then
          ierr=1
          return
        end if
        ln=ln+1
      end if
c
c ****** Discard blank lines.
c
      ll=lenstr(line)
      if (ll.eq.0) go to 100
c
c ****** Check if it is a continued "#define" line.
c
      if (defcont) then
        if (line(ll:ll).eq.bslash) then
          call addtobuf (line(1:ll-1),buf,ix,ierr)
          if (ierr.ne.0) go to 900
          go to 100
        else
          defcont=.false.
          call addtobuf (line(1:ll),buf,ix,ierr)
          if (ierr.ne.0) go to 900
          go to 300
        end if
      else
        if (ll.ge.8.and.line(1:7).eq.'#define'.and.
     &      line(ll:ll).eq.bslash) then
          defcont=.true.
          call addtobuf (line(1:ll-1),buf,ix,ierr)
          if (ierr.ne.0) go to 900
          rdfil=.true.
          go to 100
        end if
      end if
c
c ****** If it is a comment, write it out to unit IUNO
c ****** and process the next line.
c
      if (line(1:1).eq.'c'.or.line(1:1).eq.'C'.or.line(1:1).eq.'!') then
        if (iuno.gt.0) write (iuno,'(a)') line(1:ll)
        rdfil=.true.
        go to 100
      end if
c
c ****** Find the position of the first non-blank character.
c
      ixfc=fchar(line(1:ll))
c
c ****** Check length of line.
c
c      if (ll.gt.72) then
c        write (*,*)
c        write (*,*) '### WARNING from GETSTMT:'
c        write (*,*) '### Line exceeds 72 characters.'
c        write (*,*) 'Number of characters in line = ',ll
c        write (*,*) 'Line number = ',ln
c        write (*,*) 'The line is:'
c        write (*,*) line(1:ll)
c      end if
c
c ****** Add the line to the buffer.
c
      call addtobuf (line(ixfc:ll),buf,ix,ierr)
      if (ierr.ne.0) go to 900
c
c ****** Read the next line to check if it is a continuation.
c
  200 continue
c
      if (.not.rdlin(iun,line)) then
        rdfil=.true.
        go to 300
      end if
c
      ln=ln+1
c
c ****** Discard blank lines.
c
      ll=lenstr(line)
      if (ll.eq.0) go to 200
c
c ****** Check if it is a comment.
c
      if (line(1:1).eq.'c'.or.line(1:1).eq.'C'.or.line(1:1).eq.'!') then
        rdfil=.false.
        return
      end if
c
c ****** Check if it is a continuation.
c
      if (ll.ge.6.and.line(1:5).eq.' '.and.line(6:6).ne.' ') then
        call addtobuf (line(7:ll),buf,ix,ierr)
        if (ierr.ne.0) go to 900
        go to 200
      else
        rdfil=.false.
      end if
c
  300 continue
c
      return
c
  900 continue
c
c ****** Error: output buffer overflow.
c
      write (*,*)
      write (*,*) '### ERROR in GETSTMT:'
      write (*,*) '### Line buffer overflow.'
      write (*,*) '[A statement may be too long.]'
      write (*,*) 'Line number = ',ln
      call exit (1)
c
      end
c#######################################################################
      logical function rdlin (iun,line)
c
c-----------------------------------------------------------------------
c
c ****** Read a line of text from unit IUN.
c
c ****** If a line was read successfully, return with
c ****** RDLIN=.true.; otherwise, set RDLIN=.false.
c
c-----------------------------------------------------------------------
c
      character*(*) line
c
c-----------------------------------------------------------------------
c
      read (iun,'(a)',end=100,err=100) line
c
      rdlin=.true.
      return
c
  100 continue
c
c ****** Error while reading the line.
c
      rdlin=.false.
c
      return
      end
c#######################################################################
      logical function macdef (buf)
c
c-----------------------------------------------------------------------
c
c ****** Check if the buffer BUF holds a macro definition.
c
c-----------------------------------------------------------------------
c
c ****** If this is a valid macro definition, add it to the
c ****** macro tables.
c
c ****** A valid macro has the following definition:
c ******
c ******    #define <name>(arg1[,arg2]*) <replacement text>
c ******
c ****** The line can be continued by a "\".  The "#define"
c ****** must start in column 1.  The macro can have 1 or more
c ****** arguments. A "##" in the replacement text concatenates
c ****** its neighbors.
c
c-----------------------------------------------------------------------
c
      use macro_defs
c
c-----------------------------------------------------------------------
c
      character*(*) buf
c
c-----------------------------------------------------------------------
c
      character*(LMNAM) name
c
      parameter (nargmx=20)
      character*(LMARG) args(nargmx),targ
c
c-----------------------------------------------------------------------
c
      external getname,fchar,find
      logical getname,find
      integer fchar
      logical :: delim
c
c-----------------------------------------------------------------------
c
      character*3 ch3
c
c-----------------------------------------------------------------------
c
      macdef=.false.
c
      narg=0
      lb=len(buf)
c
      if (.not.(lb.gt.8.and.buf(1:8).eq.'#define ')) return
      ix=9
      ix0=ix
      if (.not.getname(buf,ix,name,0,delim)) return
      ln=lenstr(name)
      if (ix-1-ln.ge.ix0.and.buf(ix0:ix-1-ln).ne.' ') return
      ix0=ix
      if (.not.find(buf,'(',ix)) return
      if (ix-1.ge.ix0.and.buf(ix0:ix-1).ne.' ') return
      ix=ix+1
c
      nmac=nmac+1
c
      if (nmac.gt.n_macros_max) then
        write (*,*)
        write (*,*) '### ERROR in MACDEF:'
        write (*,*) '### The maximum allowed number of macros'//
     &              ' has been exceeded:'
        write (*,*) '### Maximum number allowed = ',n_macros_max
        write (*,*)
        write (*,*) '### The input line is:'
        write (*,*) trim(buf)
        call exit (1)
      end if
c
      macnam(nmac)=name
c
c ****** Collect the arguments.
c
  100 continue
c
      ix0=ix
      ifnd=ifind(buf,')',',',ix)
      if (ifnd.eq.0) then
        nmac=nmac-1
        return
      end if
      narg=narg+1
      if (narg.gt.nargmx) then
        write (*,*)
        write (*,*) '### ERROR in MACDEF:'
        write (*,*) '### Too many macro arguments.'
        write (*,*) 'Argument number = ',narg
        write (*,*) 'Input line is:'
        write (*,*) buf
        call exit (1)
      end if
      larg=ix-ix0
      if (larg.gt.LMARG) then
        write (*,*)
        write (*,*) '### ERROR in MACDEF:'
        write (*,*) '### Macro argument is too long.'
        write (*,*) 'Argument number = ',narg
        write (*,*) 'Argument length = ',larg
        write (*,*) 'Input line is:'
        write (*,*) buf
        call exit (1)
      end if
      args(narg)=buf(ix0:ix-1)
      args(narg)=adjustl(args(narg))
      ix=ix+1
      if (ifnd.eq.2) go to 100
c
      macnarg(nmac)=narg
c
c ****** Store the replacement text, flagging the arguments
c ****** with $<argnum>, where <argnum> is a 2-digit number.
c
c ****** Remove leading spaces.
c
      ic0=fchar(buf(ix:))
      ix=ix+ic0-1
c
      ixr=1
      macrpl(nmac)=' '
c
  200 continue
c
      ix0=ix
      if (getname(buf,ix,targ,0,delim)) then
        iarg=match(args,narg,targ)
        if (iarg.ne.0) then
c
c ****** Write the text and code the argument.
c
          la=lenstr(targ)
          call addtobuf (buf(ix0:ix-1-la),macrpl(nmac),ixr,ierr)
          if (ierr.ne.0) go to 900
          ch3(1:1)='$'
          write (ch3(2:3),'(i2.2)') iarg
          call addtobuf (ch3,macrpl(nmac),ixr,ierr)
          if (ierr.ne.0) go to 900
c
        else
c
c ****** Write the text.
c
          call addtobuf (buf(ix0:ix-1),macrpl(nmac),ixr,ierr)
          if (ierr.ne.0) go to 900
c
        end if
        go to 200
      else
        call addtobuf (buf(ix0:lb),macrpl(nmac),ixr,ierr)
        if (ierr.ne.0) go to 900
      end if
c
c ****** Resolve the concatenation operator "##".
c
      call filtnn (macrpl(nmac)(1:ixr-1))
c
      macdef=.true.
c
      return
c
c ****** Error: replacement text is too long.
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in MACDEF:'
      write (*,*) '### Macro replacement text is too long.'
      write (*,*) 'Macro number = ',nmac
      write (*,*) 'Replacement text:'
      write (*,*) macrpl(nmac)
      write (*,*) 'Input line is:'
      write (*,*) buf
      call exit (1)
c
      end
c#######################################################################
      subroutine filtnn (line)
c
c-----------------------------------------------------------------------
c
c ****** Filter the ## operator.
c
c-----------------------------------------------------------------------
c
      character*(*) line
c
c-----------------------------------------------------------------------
c
      external fchar
      integer fchar
c
c-----------------------------------------------------------------------
c
      ll=lenstr(line)
c
      ix=1
  100 continue
      if (ix+1.gt.ll) go to 200
      if (line(ix:ix+1).eq.'##') then
        ln=lenstr(line(1:ix-1))
        ir=max0(0,fchar(line(ix+2:))-1)
        nd=ix-1-ln+2+ir
        line(ln+1:)=line(ln+1+nd:)
        ll=ll-nd
        ix=ln+1
      else
        ix=ix+1
      end if
      go to 100
c
  200 continue
c
      return
      end
c#######################################################################
      logical function macres (line,obuf,ixo)
c
c-----------------------------------------------------------------------
c
c ****** Resolve the text in LINE for macro calls.
c
c-----------------------------------------------------------------------
c
      use params
      use macro_defs
c
c-----------------------------------------------------------------------
c
      character*(*) line,obuf
c
c-----------------------------------------------------------------------
c
      character*(LMNAM) name
c
      parameter (nargmx=20)
      character*(LMARG) args(nargmx)
      logical :: delim
c
c-----------------------------------------------------------------------
c
      logical, external :: getname
c
c-----------------------------------------------------------------------
c
      macres=.false.
c
c ****** If no macros are defined, just copy the line to the
c ****** output buffer and return.
c
      if (nmac.le.0) then
        call addtobuf (line,obuf,ixo,ierr)
        if (ierr.ne.0) go to 900
        return
      end if
c
      ix=1
c
  100 continue
c
      ix0=ix
c
      if (getname(line,ix,name,0,delim)) then
c
c ****** Write the text up to the name.
c
        ln=lenstr(name)
        call addtobuf (line(ix0:ix-1-ln),obuf,ixo,ierr)
        if (ierr.ne.0) go to 900
        ix0=ix-ln
c
c ****** Check if this is a macro call.
c
        imac=match(macnam,nmac,name)
c
        if (imac.ne.0) then
c
c ****** Get the arguments.
c
          ixhold=ix
          call getargs (line,ix,nargmx,args,narg,ierr)
          if (ierr.ne.0) then
            call addtobuf (name(1:ln),obuf,ixo,ierr)
            if (ierr.ne.0) go to 900
            go to 100
          end if
c
          if (idebug.ge.3) then
            write (*,*)
            write (*,*) '### Found a macro call:'
            write (*,*) 'Macro name = ',name
            write (*,*) 'Number of args = ',narg
            write (*,*) 'Arguments:'
            do 200 i=1,narg
              write (*,*) 'ARG#',i,': ',args(i)
  200       continue
          end if
c
c ****** Put in the replacement text.
c
          call putrpl (imac,args,narg,obuf,ixo,ierr)
          if (ierr.ne.0) then
            call addtobuf (name(1:ln),obuf,ixo,ierr)
            if (ierr.ne.0) go to 900
            ix=ixhold
            go to 100
          end if
c
          macres=.true.
c
        else
c
c ****** Write the name.
c
          call addtobuf (name(1:ln),obuf,ixo,ierr)
          if (ierr.ne.0) go to 900
c
        end if
        go to 100
c
      else
c
c ****** Write the rest of the line.
c
        call addtobuf (line(ix0:ix-1),obuf,ixo,ierr)
        if (ierr.ne.0) go to 900
c
      end if
c
      return
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in MACRES:'
      write (*,*) '### Output buffer overflow.'
      call exit (1)
c
      end
c#######################################################################
      subroutine getargs (line,ixi,nargmx,args,narg,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Get the arguments for a macro call.
c
c-----------------------------------------------------------------------
c
      character*(*) line,args(nargmx)
c
c-----------------------------------------------------------------------
c
      external find
      logical find,found
c
c-----------------------------------------------------------------------
c
      ierr=1
c
      l_arg=len(args(1))
      narg=0
      ix=ixi
c
c ****** Find the first left parenthesis.
c
      ix0=ix
      found=find(line,'(',ix)
      if (.not.found) return
c
c ****** Check that it is the first non-blank character.
c
      if (ix-1.ge.ix0.and.line(ix0:ix-1).ne.' ') return
c
      ixl=ix
      ix=ix+1
c
c ****** Find the first right parenthesis.
c
      found=find(line,')',ix)
      if (.not.found) return
c
      ixr=ix
c
c ****** Get the arguments.
c
      ix=ixl+1
  100 continue
      ix0=ix
      found=find(line(1:ixr-1),',',ix)
      if (.not.found) ix=ixr
      narg=narg+1
      if (narg.gt.nargmx) then
        write (*,*)
        write (*,*) '### ERROR in GETARGS:'
        write (*,*) '### Too many macro arguments.'
        write (*,*) 'Argument number = ',narg
        write (*,*) 'Line is:'
        write (*,*) line
        call exit (1)
      end if
      larg=lenstr(line(ix0:ix-1))
      if (larg.gt.l_arg) then
        write (*,*)
        write (*,*) '### ERROR in GETARGS:'
        write (*,*) '### Macro argument is too long.'
        write (*,*) 'Argument number = ',narg
        write (*,*) 'Argument length = ',larg
        write (*,*) 'Line is:'
        write (*,*) line
        call exit (1)
      end if
      args(narg)=line(ix0:ix-1)
      args(narg)=adjustl(args(narg))
      if (found) then
        ix=ix+1
        go to 100
      end if
c
c ****** Arguments found successfully.
c
      ixi=ixr+1
      ierr=0
c
      return
      end
c#######################################################################
      subroutine putrpl (imac,args,narg,buf,ix,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Put the replacement text for a macro call into
c ****** the output buffer BUF.
c
c ****** The NARG actual arguments are in array ARGS.
c
c ****** IMAC is the macro name index.
c
c-----------------------------------------------------------------------
c
      use macro_defs
c
c-----------------------------------------------------------------------
c
      integer :: imac
      integer :: narg
      character*(*), dimension(narg):: args
      character*(*) :: buf
      integer :: ix
      integer :: ierr
c
c-----------------------------------------------------------------------
c
      ierr=1
c
c ****** Check the number of arguments.
c
      if (narg.ne.macnarg(imac)) return
c
c ****** Use the macro replacement text.
c
      ll=lenstr(macrpl(imac))
      ir=1
  100 continue
      if (ir.gt.ll) go to 200
c
c ****** Check if this is an argument.
c
      if (macrpl(imac)(ir:ir).eq.'$') then
c
c ****** Get the argument number.
c
        read (macrpl(imac)(ir+1:ir+2),'(i2)') iarg
        larg=lenstr(args(iarg))
c
c ****** Substitute the actual argument.
c
        call addtobuf (args(iarg)(1:larg),buf,ix,ierr)
        if (ierr.ne.0) go to 900
c
        ir=ir+3
c
      else
c
c ****** Write a character of the replacement text.
c
        call addtobuf (macrpl(imac)(ir:ir),buf,ix,ierr)
        if (ierr.ne.0) go to 900
        ir=ir+1
c
      end if
      go to 100
c
  200 continue
c
      ierr=0
c
      return
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in PUTRPL:'
      write (*,*) '### Output buffer overflow.'
      call exit (1)
c
      end
c#######################################################################
      subroutine set_parameters (cname,cvers,cdate)
c
c-----------------------------------------------------------------------
c
c ****** Set parameters from the command-line arguments.
c
c-----------------------------------------------------------------------
c
      use syntax
      use paragraph_def
      use get_usage_line_interface
      use print_par_interface
      use delete_par_interface
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: cname,cvers,cdate
c
c-----------------------------------------------------------------------
c
c ****** Storage for the usage line.
c
      type(paragraph), pointer :: usage
c
c ****** Storage for the error message.
c
      character(512) :: errmsg
c
c-----------------------------------------------------------------------
c
      integer :: ierr
      character(512) :: arg
      logical :: set
c
c-----------------------------------------------------------------------
c
      integer, external :: intval
c
c-----------------------------------------------------------------------
c
c ****** Define the syntax.
c
      call defarg (GROUP_KA,'-debug','0','<level>')
      call defarg (GROUP_KA,'-macdef','<no_default>','<file>')
      call defarg (GROUP_A ,'infile',' ',' ')
      call defarg (GROUP_A ,'outfile',' ',' ')
c
c ****** Parse the command line.
c
      call parse (errmsg,ierr)
c
      if (ierr.ne.0) then
c
        write (*,*)
        write (*,*) '### ',cname,' Version ',cvers,' of ',cdate,'.'
        write (*,*) '### Expand macros in a source file.'
c
        if (ierr.gt.1) then
          write (*,*)
          write (*,*) errmsg
        end if
c
c ****** Print the usage line.
c
        call get_usage_line (usage)
c
        write (*,*)
        write (*,*) 'Usage:'
        write (*,*)
c
        call print_par (usage)
c
        write (*,*)
        write (*,*) 'The macros in <infile> are expanded, sending'//
     &              ' the output to <outfile>.'
        write (*,*)
        write (*,*) 'Use -debug to get diagnostic information:'
        write (*,*) '  debug.ge.1: lists macro definitions'
        write (*,*) '  debug.ge.2: lists macro call replacement text'
        write (*,*) '  debug.ge.3: lists every line read and'//
     &              ' detailed call diagnostic output'
        write (*,*)
        write (*,*) 'Use -macdef to specify a separate file of'//
     &              ' macro definitions.  If this file is'
        write (*,*) 'specified, it is read first, before'//
     &              ' reading the input source file.'
c
        call delete_par (usage)
c
        call exit (1)
c
      end if
c
c ****** Set the parameters.
c
c ****** Debug level.
c
      call fetcharg ('-debug',set,arg)
      idebug=intval(trim(arg),'-debug')
c
c ****** Macro definitions file.
c
      call fetcharg ('-macdef',set,arg)
      if (set) then
        macro_def_file=trim(arg)
      else
        macro_def_file=' '
      end if
c
c ****** Input file name.
c
      call fetcharg ('infile',set,arg)
      infile=trim(arg)
c
c ****** Output file name.
c
      call fetcharg ('outfile',set,arg)
      outfile=trim(arg)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c ****** Source to build the parsing library.
c ****** These routines are used by Zoran Mikic's tools.
c
c-----------------------------------------------------------------------
c
c        07/29/2003, ZM, Version 1.00:
c
c         - Original version of the parsing library.
c           This library was put together to facilitate the
c           development of ZM's tools.
c           It includes routines to parse the command line.
c           The code was cleaned up to use standard FORTRAN90.
c
c        01/17/2005, ZM, Version 1.01:
c
c         - Added the function NARGS_SPECIFIED to return the
c           number of arguments specified on the command line.
c
c        10/28/2005, ZM, Version 1.02:
c
c         - Added the functions LOAD_LIST_OF_REALS and
c           LOAD_LIST_OF_INTS that can be used to parse
c           arguments that contain lists of real or integer values.
c         - Changed the length of temporary arguments to equal
c           512 characters to accomodate long file names.
c
c        10/31/2006, ZM, Version 1.03:
c
c         - Removed the EXTERNAL declarations for GETARG and IARGC
c           since these are now intrinsic routines in the
c           Intel 9.1 compiler.
c
c        03/10/2008, ZM, Version 1.04:
c
c         - Added the LCASE and UCASE functions to convert strings
c           to lowercase and uppercase.
c
c-----------------------------------------------------------------------
c
c#######################################################################
      module parselib_ident
c
      character(*), parameter :: cname='PARSELIB'
      character(*), parameter :: cvers='1.04'
      character(*), parameter :: cdate='03/10/2008'
c
      end module
c#######################################################################
      module parse_args
c
      use string_def
c
      implicit none
c
c-----------------------------------------------------------------------
c ****** Argument descriptor and storage for command-line arguments.
c-----------------------------------------------------------------------
c
c ****** Structure to hold an argument.
c
      type :: arg_descriptor
        integer :: group
        logical :: set
        logical :: required
        type(string) :: keyword
        type(string) :: name
        type(string) :: value
      end type
c
c ****** Maximum number of arguments.
c
      integer, parameter :: mxarg=100
c
c ****** Number of arguments defined.
c
      integer :: nargs
c
c ****** Argument descriptor.
c
      type(arg_descriptor), dimension(mxarg) :: args
c
c ****** Number of arguments specified.
c
      integer :: nargs_spec
c
      end module
c#######################################################################
      subroutine ffopen (iun,fname,mode,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Open file FNAME and link it to unit IUN.
c
c-----------------------------------------------------------------------
c
c ****** When MODE='r', the file must exist.
c ****** When MODE='w', the file is created.
c ****** When MODE='rw', the file must exist, but can be overwritten.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: iun
      character(*) :: fname
      character(*) :: mode
      integer :: ierr
c
c-----------------------------------------------------------------------
c
      ierr=0
c
      if (mode.eq.'r') then
        open (iun,file=fname,status='old',err=900)
      else if (mode.eq.'rw') then
        open (iun,file=fname,status='replace',err=900)
      else if (mode.eq.'w') then
        open (iun,file=fname,status='new',err=900)
      else
        write (*,*)
        write (*,*) '### ERROR in FFOPEN:'
        write (*,*) '### Invalid MODE requested.'
        write (*,*) 'MODE = ',mode
        write (*,*) 'File name: ',trim(fname)
        ierr=2
        return
      end if
c
      return
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in FFOPEN:'
      write (*,*) '### Error while opening the requested file.'
      write (*,*) 'File name: ',trim(fname)
      write (*,*) 'MODE = ',mode
      ierr=1
c
      return
      end
c#######################################################################
      function lcase (s)
c
c-----------------------------------------------------------------------
c
c ****** Convert the string S into lowercase letters and return it as
c ****** the function result.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*), intent(in) :: s
      character(len(s)) :: lcase
c
c-----------------------------------------------------------------------
c
      integer :: i,ic
c
c-----------------------------------------------------------------------
c
      lcase=' '
c
      do i=1,len_trim(s)
        ic=iachar(s(i:i))
        if (ic.ge.65.and.ic.le.90) then
          ic=ic+32
        end if
        lcase(i:i)=achar(ic)
      end do
c
      return
      end
c#######################################################################
      function ucase (s)
c
c-----------------------------------------------------------------------
c
c ****** Convert the string S into uppercase letters and return it as
c ****** the function result.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*), intent(in) :: s
      character(len(s)) :: ucase
c
c-----------------------------------------------------------------------
c
      integer :: i,ic
c
c-----------------------------------------------------------------------
c
      ucase=' '
c
      do i=1,len_trim(s)
        ic=iachar(s(i:i))
        if (ic.ge.97.and.ic.le.122) then
          ic=ic-32
        end if
        ucase(i:i)=achar(ic)
      end do
c
      return
      end
c#######################################################################
      subroutine parse (errmsg,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Parse the command line.
c
c-----------------------------------------------------------------------
c
c ****** The syntax for the keyword/argument items can be defined
c ****** by using routine DEFARG.
c
c ****** On return, IERR=0 indicates that the command line was
c ****** parsed successfully.
c
c ****** IERR=1 indicates that no arguments were present.  This
c ****** is usually used to print the usage line.
c
c ****** IERR=2 indicates that a syntax error occured.
c
c ****** IERR=3 indicates that one or more required arguments
c ****** was not supplied.
c
c ****** When IERR=2 or IERR=3, an error message is put into
c ****** character string ERRMSG.
c
c-----------------------------------------------------------------------
c
      use syntax
      use parse_args
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: errmsg
      integer :: ierr
c
c-----------------------------------------------------------------------
c
c ****** Command line arguments.
c
      integer :: iargc
c
c-----------------------------------------------------------------------
c
      integer, external :: matchkw
      integer, external :: nextarg
c
c-----------------------------------------------------------------------
c
      character(512) :: arg
      integer :: na,ia,ia0,iarg,ls,i
c
c-----------------------------------------------------------------------
c
c ****** Initialization.
c
      ierr=0
      nargs_spec=0
      errmsg=' '
c
c ****** Get the number of command line arguments.
c
      na=iargc()
      if (na.eq.0) then
        ierr=1
        go to 900
      end if
c
      ia=1
  200 continue
c
      ia0=ia
c
c ****** Process arguments with syntax: <kw> <arg>
c
      if (na-ia+1.ge.2) then
        call getarg (ia,arg)
        iarg=matchkw(GROUP_KA,trim(arg))
        if (iarg.gt.0) then
          if (.not.args(iarg)%set) then
            ia=ia+1
            call getarg (ia,arg)
            call delete_str (args(iarg)%value)
            call put_str (trim(arg),args(iarg)%value)
            args(iarg)%set=.true.
            ia=ia+1
            nargs_spec=nargs_spec+1
            go to 300
          end if
        end if
      end if
c
c ****** Process arguments with syntax: <kw> <arg> <arg>
c
      if (na-ia+1.ge.3) then
        call getarg (ia,arg)
        iarg=matchkw(GROUP_KAA,trim(arg))
        if (iarg.gt.0) then
          if (.not.args(iarg)%set) then
            ia=ia+1
            call getarg (ia,arg)
            ls=len_trim(arg)
            ls=ls+1
            arg(ls:ls)=' '
            ia=ia+1
            call getarg (ia,arg(ls+1:))
            call delete_str (args(iarg)%value)
            call put_str (trim(arg),args(iarg)%value)
            args(iarg)%set=.true.
            ia=ia+1
            nargs_spec=nargs_spec+1
            go to 300
          end if
        end if
      end if
c
c ****** Process arguments with syntax: <kw>
c
      if (na-ia+1.ge.1) then
        call getarg (ia,arg)
        iarg=matchkw(GROUP_K,trim(arg))
        if (iarg.gt.0) then
          if (.not.args(iarg)%set) then
            call delete_str (args(iarg)%value)
            call put_str (' ',args(iarg)%value)
            args(iarg)%set=.true.
            ia=ia+1
            nargs_spec=nargs_spec+1
            go to 300
          end if
        end if
      end if
c
c ****** Process arguments with syntax: <arg>
c
      if (na-ia+1.ge.1) then
        iarg=nextarg(GROUP_A)
        if (iarg.gt.0) then
          call getarg (ia,arg)
          call delete_str (args(iarg)%value)
          call put_str (trim(arg),args(iarg)%value)
          args(iarg)%set=.true.
          ia=ia+1
          nargs_spec=nargs_spec+1
          go to 300
        end if
      end if
c
  300 continue
c
c ****** Check that an argument was found.
c
      if (ia.eq.ia0) then
        ierr=2
        go to 900
      end if
c
c ****** Keep processing arguments until done.
c
      if (na-ia+1.gt.0) go to 200
c
c ****** Check that the required arguments were supplied.
c
      do i=1,nargs
        if (args(i)%required.and..not.args(i)%set) then
          ierr=3
          go to 900
        end if
      enddo
c
      return
c
c ****** Error exit.
c
  900 continue
c
      if (ierr.eq.2) then
        errmsg='### Syntax error.'
      else if (ierr.eq.3) then
        errmsg='### A required argument was not supplied.'
      end if
c
      return
      end
c#######################################################################
      subroutine get_usage_line (usage)
c
c-----------------------------------------------------------------------
c
c ****** Construct the usage line in paragraph USAGE.
c
c ****** Use routine PRINT_PAR to write the usage line.
c
c-----------------------------------------------------------------------
c
      use parse_args
      use paragraph_def
      use new_par_interface
      use add_line_interface
      use get_str_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: usage
c
c-----------------------------------------------------------------------
c
c ****** Right margin for printing the usage line.
c
      integer, parameter :: rmargin=78
c
c-----------------------------------------------------------------------
c
      character(512) :: line
      integer :: iarg,n0
      type(paragraph), pointer :: current_par
c
c-----------------------------------------------------------------------
c
c ****** Construct the usage line in USAGE.
c
      call new_par (usage)
      current_par=>usage
c
c ****** Start with the command name (as invoked).
c
      call getarg (0,line)
c
      iarg=1
c
c ****** Add the arguments.
c
      do while (iarg.le.nargs)
c
c ****** Add the syntax for the next argument to LINE.
c
        n0=len_trim(line)
c
        if (args(iarg)%required) then
          line=trim(line)//' '//get_str(args(iarg)%keyword)
        else
          line=trim(line)//' ['//get_str(args(iarg)%keyword)
        end if
        line=trim(line)//' '//get_str(args(iarg)%name)
        if (.not.args(iarg)%required) then
          line=trim(line)//']'
        end if
c
c ****** Check if the addition of the argument causes the line
c ****** to wrap; if it does, break the line prior to the
c ****** argument text.
c
        if (len_trim(line).gt.rmargin) then
          call add_line (line(1:n0),current_par)
          line=' '//line(n0+1:)
        end if
c
c ****** If the line is still too long, force a break at RMARGIN
c ****** until the line is shorter than RMARGIN.
c
        do while (len_trim(line).gt.rmargin)
          call add_line (line(1:rmargin),current_par)
          line='  '//line(rmargin+1:)
        enddo
c
c ****** Process the next argument.
c
        iarg=iarg+1
c
      enddo
c
c ****** Add the last line to the paragraph.
c
      if (line.ne.' ') call add_line (trim(line),current_par)
c
      return
      end
c#######################################################################
      subroutine defarg (group,keyword,default,name)
c
c-----------------------------------------------------------------------
c
c ****** Define the syntax for a command line argument item.
c
c-----------------------------------------------------------------------
c
c ****** GROUP is the syntax group index;
c ****** KEYWORD is the keyword;
c ****** DEFAULT is the default value of the argument;
c ****** NAME is the name of the argument (for use in
c ****** constructing the usage line).
c
c-----------------------------------------------------------------------
c
      use syntax
      use parse_args
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: group
      character(*) :: keyword
      character(*) :: default
      character(*) :: name
c
c-----------------------------------------------------------------------
c
c ****** Check that the group index is valid.
c
      if (group.lt.0.or.group.gt.ngroups) then
        write (*,*)
        write (*,*) '### ERROR in DEFARG:'
        write (*,*) '### An invalid group index was specified.'
        write (*,*) 'Group index = ',group
        write (*,*) 'Keyword = ',trim(keyword)
        if (name.ne.' ') write (*,*) 'Name = ',trim(name)
        write (*,*)
        write (*,*) '### This indicates a programming error'//
     &              ' in the syntax definition and use.'
        call exit (1)
      end if
c
c ****** Check for a null keyword.
c
      if (keyword.eq.' ') then
        write (*,*)
        write (*,*) '### ERROR in DEFARG:'
        write (*,*) '### The keyword is null.'
        write (*,*) 'Group index = ',group
        if (name.ne.' ') write (*,*) 'Name = ',trim(name)
        write (*,*)
        write (*,*) '### This indicates a programming error'//
     &              ' in the syntax definition and use.'
        call exit (1)
      end if
c
c ****** Increment the argument counter.
c
      if (nargs.ge.mxarg) then
        write (*,*)
        write (*,*) '### ERROR in DEFARG:'
        write (*,*) '### Exceeded the number of allowed arguments.'
        write (*,*) 'Maximum number of arguments = ',mxarg
        write (*,*) 'Group index = ',group
        write (*,*) 'Keyword = ',trim(keyword)
        if (name.ne.' ') write (*,*) 'Name = ',trim(name)
        write (*,*)
        write (*,*) '### This indicates a programming error'//
     &              ' in the syntax definition and use.'
        call exit (1)
      end if
c
      nargs=nargs+1
c
c ****** Store the group index and keyword.
c
c ****** For group GROUP_A (single arguments), the name of the
c ****** argument is passed as the "keyword".
c
      args(nargs)%group=group
      call put_str (trim(keyword),args(nargs)%keyword)
c
c ****** Initialize the flag that indicates whether an argument
c ****** has been set.
c
      args(nargs)%set=.false.
c
c ****** If a default argument was supplied, the argument
c ****** does not have to be set.  Use DEFAULT=' ' to
c ****** indicate that an argument is required.
c
c ****** If a default argument has been supplied, store it in
c ****** ARGS(nargs)%VALUE.  If there is no default,
c ****** set ARGS(nargs)%VALUE to an empty string.
c
c ****** Since group GROUP_K doesn't have an argument,
c ****** DEFAULT is ignored for this group.
c
      if (group.eq.GROUP_K) then
        args(nargs)%required=.false.
        call put_str (' ',args(nargs)%value)
      else
        if (default.eq.' ') then
          args(nargs)%required=.true.
          call put_str (' ',args(nargs)%value)
        else
          args(nargs)%required=.false.
          call put_str (trim(default),args(nargs)%value)
        end if
      end if
c
c ****** Store the argument name.  For groups GROUP_K (keywords)
c ****** and GROUP_A (single arguments), there is no argument name,
c ****** so NAME is ignored.
c
      if (group.eq.GROUP_K.or.group.eq.GROUP_A) then
        call put_str (' ',args(nargs)%name)
      else
        call put_str (trim(name),args(nargs)%name)
      end if
c
      return
      end
c#######################################################################
      subroutine fetcharg (keyword,set,arg)
c
c-----------------------------------------------------------------------
c
c ****** Fetch the value of the argument corresponding to
c ****** keyword KEYWORD.
c
c-----------------------------------------------------------------------
c
c ****** If KEYWORD is a keyword-type argument (GROUP_K), return
c ****** its setting through variable SET.  The variable ARG should
c ****** be ignored for this type of keyword.
c
c ****** For keywords with arguments (GROUP_A, GROUP_KA, and
c ****** GROUP_KAA), return the value of the arguments in ARG,
c ****** and return SET=.true. if they were set via the command line;
c ****** otherwise, return SET=.false..
c
c-----------------------------------------------------------------------
c
      use parse_args
      use get_str_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: keyword
      logical :: set
      character(*) :: arg
c
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c
      do i=nargs,1,-1
        if (keyword.eq.get_str(args(i)%keyword)) go to 100
      enddo
c
      write (*,*)
      write (*,*) '### ERROR in FETCHARG:'
      write (*,*) '### The requested keyword could not be matched.'
      write (*,*) 'Keyword = ',trim(keyword)
      write (*,*)
      write (*,*) '### This indicates a programming error'//
     &            ' in the syntax definition and use.'
      call exit (1)
c
  100 continue
c
      set=args(i)%set
      arg=get_str(args(i)%value)
c
      return
      end
c#######################################################################
      function matchkw (group,keyword)
c
c-----------------------------------------------------------------------
c
c ****** Match keyword KEYWORD against the list of keywords in
c ****** group GROUP.
c
c ****** If found, set the function value to the corresponding
c ****** argument number.  Otherwise, return MATCHKW=0.
c
c-----------------------------------------------------------------------
c
      use parse_args
      use get_str_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: group
      character(*) :: keyword
      integer :: matchkw
c
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c
      matchkw=0
c
      do i=nargs,1,-1
        if (group.eq.args(i)%group) then
          if (keyword.eq.get_str(args(i)%keyword)) then
            matchkw=i
            return
          end if
        end if
      enddo
c
      return
      end
c#######################################################################
      function nextarg (group)
c
c-----------------------------------------------------------------------
c
c ****** Find the position of the next argument in group GROUP
c ****** that has not been set.
c
c-----------------------------------------------------------------------
c
c ****** If an empty slot is found, set the function value
c ****** to the corresponding argument number.
c
c ****** Otherwise, return NXTARG=0.
c
c-----------------------------------------------------------------------
c
      use parse_args
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: group
      integer :: nextarg
c
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c
      nextarg=0
c
      do i=1,nargs
        if (group.eq.args(i)%group) then
          if (.not.args(i)%set) then
            nextarg=i
            return
          end if
        end if
      enddo
c
      return
      end
c#######################################################################
      subroutine nargs_specified (n)
c
c-----------------------------------------------------------------------
c
c ****** Return the number of arguments specified on the command
c ****** line.
c
c-----------------------------------------------------------------------
c
      use parse_args
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: n
c
c-----------------------------------------------------------------------
c
      n=nargs_spec
c
      return
      end
c#######################################################################
      subroutine new_par (par)
c
c-----------------------------------------------------------------------
c
c ****** Initialize paragraph PAR.
c
c-----------------------------------------------------------------------
c
      use paragraph_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: par
c
c-----------------------------------------------------------------------
c
      allocate (par)
      nullify (par%line%c)
      nullify (par%next)
c
      return
      end
c#######################################################################
      subroutine delete_par (par)
c
c-----------------------------------------------------------------------
c
c ****** Delete paragraph PAR and deallocate its storage and that
c ****** of its linked lists.
c
c-----------------------------------------------------------------------
c
      use paragraph_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: par
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: current_par,previous_par
c
c-----------------------------------------------------------------------
c
      current_par=>par
c
      do
c
c ****** Deallocate the line buffer.
c
        call delete_str (current_par%line)
c
c ****** Set the pointer to the next line (if it has been defined).
c
        if (.not.associated(current_par%next)) exit
        previous_par=>current_par
        current_par=>current_par%next
        deallocate (previous_par)
c
      enddo
c
      deallocate (current_par)
c
      return
      end
c#######################################################################
      subroutine add_line (line,par)
c
c-----------------------------------------------------------------------
c
c ****** Add LINE to paragraph PAR.
c
c ****** On exit from this routine, PAR points to a new line,
c ****** and can be used to store the next line of text.
c
c-----------------------------------------------------------------------
c
      use paragraph_def
      use new_par_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: line
      type(paragraph), pointer :: par
c
c-----------------------------------------------------------------------
c
c ****** Store LINE into the string buffer for the current line.
c
      call put_str (line,par%line)
c
c ****** Allocate a pointer to the next line.
c
      call new_par (par%next)
c
c ****** Set PAR to point to the next line.
c
      par=>par%next
c
      return
      end
c#######################################################################
      subroutine print_par (par)
c
c-----------------------------------------------------------------------
c
c ****** Print all lines of paragraph PAR to STDOUT.
c
c-----------------------------------------------------------------------
c
      use paragraph_def
      use get_str_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: par
c
c-----------------------------------------------------------------------
c
      type(paragraph), pointer :: current_par
c
c-----------------------------------------------------------------------
c
      current_par=>par
c
      do
c
c ****** Print the line if it has been defined.
c
        if (associated(current_par%line%c)) then
          write (*,*) trim(get_str(current_par%line))
        end if
c
c ****** Set the pointer to the next line (if it has been defined).
c
        if (.not.associated(current_par%next)) exit
        current_par=>current_par%next
c
      enddo
c
      return
      end
c#######################################################################
      subroutine put_str (cval,str)
c
c-----------------------------------------------------------------------
c
c ****** Store character variable CVAL into string STR.
c ****** This routine allocates storage for the string.
c
c-----------------------------------------------------------------------
c
      use string_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: cval
      type(string) :: str
c
c-----------------------------------------------------------------------
c
      integer :: l,i
c
c-----------------------------------------------------------------------
c
      l=len(cval)
c
      allocate (str%c(l))
c
      do i=1,l
        str%c(i)=cval(i:i)
      enddo
c
      return
      end
c#######################################################################
      function get_str (str)
c
c-----------------------------------------------------------------------
c
c ****** Return the value of string STR as the function value
c ****** (as an assumed-length character variable).
c
c-----------------------------------------------------------------------
c
      use string_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(string) :: str
      character(size(str%c)) :: get_str
c
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c
      do i=1,size(str%c)
        get_str(i:i)=str%c(i)
      enddo
c
      return
      end
c#######################################################################
      subroutine delete_str (str)
c
c-----------------------------------------------------------------------
c
c ****** Delete the storage for string STR.
c
c-----------------------------------------------------------------------
c
      use string_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(string) :: str
c
c-----------------------------------------------------------------------
c
      if (associated(str%c)) then
        deallocate (str%c)
      end if
      nullify (str%c)
c
      return
      end
c#######################################################################
      function intval (avalue,name)
c
c-----------------------------------------------------------------------
c
c ****** Get the value of the integer in character variable AVALUE.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: avalue
      character(*) :: name
      integer :: intval
c
c-----------------------------------------------------------------------
c
      logical, external :: ifint
c
c-----------------------------------------------------------------------
c
      integer :: ivalue
c
c-----------------------------------------------------------------------
c
      if (.not.ifint(trim(avalue),ivalue)) then
        write (*,*)
        write (*,*) '### ERROR in INTVAL:'
        write (*,*) '### Could not interpret an integer '//
     &              'while setting: ',trim(name)
        write (*,*) 'Invalid format: ',trim(avalue)
        call exit (1)
      end if
c
      intval=ivalue
c
      return
      end
c#######################################################################
      function fpval (avalue,name)
c
c-----------------------------------------------------------------------
c
c ****** Get the value of the floating point number in character
c ****** variable AVALUE.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      character(*) :: avalue
      character(*) :: name
      real(r_typ) :: fpval
c
c-----------------------------------------------------------------------
c
      logical, external :: iffp
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: value
c
c-----------------------------------------------------------------------
c
      if (.not.iffp(trim(avalue),value)) then
        write (*,*)
        write (*,*) '### ERROR in FPVAL:'
        write (*,*) '### Could not interpret a floating point '//
     &              'number while setting: ',trim(name)
        write (*,*) 'Invalid format: ',trim(avalue)
        call exit (1)
      end if
c
      fpval=value
c
      return
      end
c#######################################################################
      function iffp (alpha,value)
c
c-----------------------------------------------------------------------
c
c ****** Determine if ALPHA represents a floating point number;
c ****** if so, return its value in VALUE.
c
c-----------------------------------------------------------------------
c
c ****** Set IFFP=.TRUE. if ALPHA contains an alphanumeric
c ****** string with the following format:
c
c       ALPHA = '[A][B...B][.][B...B][e[A]B[B...B]]',
c
c ****** where A represents a + or - sign, and B represents a digit
c ****** between 0 and 9, inclusive.
c ****** The exponent may be denoted by a lower or upper case e.
c ****** The mantissa must have at least one digit, and the
c ****** the exponent, if present, must have between 1 and 3 digits.
c ****** Otherwise, set IFFP=.FALSE.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: alpha
      real(r_typ) :: value
      logical :: iffp
c
c-----------------------------------------------------------------------
c
      integer :: nmant,nexp,k,i,ke
      logical :: ifpoint,ifexp
      character(7) :: fmt
c
c-----------------------------------------------------------------------
c
      iffp=.false.
      ifpoint=.false.
      ifexp=.false.
      nmant=0
      nexp=0
c
      do k=1,len_trim(alpha)
        i=iachar(alpha(k:k))
c
c ****** Check for a sign in the first position.
c
        if (k.eq.1.and.(i.eq.43.or.i.eq.45)) cycle
c
c ****** Check for a digit.
c
        if (i.ge.48.and.i.le.57) then
c
c ****** Count digits in mantissa and exponent.
c
        if (ifexp) then
          nexp=nexp+1
          else
            nmant=nmant+1
          end if
          cycle
c
        end if
c
c ****** Check for a decimal point.
c
        if (.not.ifpoint.and.i.eq.46) then
c
c ****** Check that we are in the mantissa.
c
          if (.not.ifexp) then
            ifpoint=.true.
            cycle
          end if
c
        end if
c
c ****** Check for an exponent.
c
        if (.not.ifexp.and.(i.eq.101.or.i.eq.69)) then
          ifexp=.true.
          ke=k
          cycle
        end if
c
c ****** Check for an exponent sign.
c
        if (ifexp.and.k.eq.ke+1.and.(i.eq.43.or.i.eq.45)) cycle
c
c ****** Failed check: fall through here.
c
        iffp=.false.
c
        return
c
      enddo
c
c ****** Final check of validity: check number of digits in
c ****** the mantissa and exponent.
c
      if (nmant.ge.1) iffp=.true.
      if (ifexp.and.(nexp.lt.1.or.nexp.gt.3)) iffp=.false.
c
c ****** Obtain its numeric value.
c
      fmt='(f  .0)'
      write (fmt(3:4),'(i2.2)') len_trim(alpha)
c
      if (iffp) read (alpha,fmt) value
c
      return
      end
c#######################################################################
      function ifint (alpha,ivalue)
c
c-----------------------------------------------------------------------
c
c ****** If ALPHA represents an integer, return IFINT=.true., and
c ****** put its value into IVALUE.
c
c ****** Otherwise, return IFINT=.false..
c
c-----------------------------------------------------------------------
c
c ****** A valid integer has the format:
c
c          ALPHA = '[A]B[B...B]',
c
c ****** where A represents a + or - sign, and B represents a digit
c ****** between 0 and 9, inclusive.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: alpha
      integer :: ivalue
      logical :: ifint
c
c-----------------------------------------------------------------------
c
      integer :: k,i
      character(5) :: fmt
c
c-----------------------------------------------------------------------
c
      ifint=.false.
c
      do k=1,len_trim(alpha)
c
        i=iachar(alpha(k:k))
c
c ****** Check for a sign in the first position.
c
        if (k.eq.1.and.(i.eq.43.or.i.eq.45)) cycle
c
c ****** Check for a digit.
c
        if (i.ge.48.and.i.le.57) then
          ifint=.true.
          cycle
        end if
c
c ****** Failed check: fall through here.
c
        ifint=.false.
c
        return
c
      enddo
c
c ****** Obtain its numeric value.
c
      fmt='(i  )'
      write (fmt(3:4),'(i2.2)') len_trim(alpha)
c
      if (ifint) read (alpha,fmt) ivalue
c
      return
      end
c#######################################################################
      subroutine load_list_of_reals (s,label,n,f)
c
c-----------------------------------------------------------------------
c
c ****** Read N real values from character string S into
c ****** array F(N). The values in S may be either space or
c ****** comma separated.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: s
      character(*) :: label
      integer :: n
      real(r_typ), dimension(n) :: f
      intent(in) :: s,label,n
      intent(out) :: f
c
c-----------------------------------------------------------------------
c
      integer :: i,i0,i1
      character :: delimiter
      character(512) :: list
c
c-----------------------------------------------------------------------
c
      real(r_typ), external :: fpval
c
c-----------------------------------------------------------------------
c
c ****** Make a local copy of the string (removing leading spaces).
c
      list=adjustl(s)
c
c ****** If any commas are present, use a comma as the delimiter.
c ****** Otherwise, one or more spaces is used as a delimiter.
c ****** In this case, compress multiple spaces into a single space.
c
      if (index(list,',').ne.0) then
        delimiter=','
      else
        delimiter=' '
        call delete_repeated_char (list,' ')
      end if
c
c ****** Read the list of N numbers sequentially into F.
c
      i0=1
      do i=1,n-1
        i1=scan(list(i0:),delimiter)+i0-2
        f(i)=fpval(adjustl(list(i0:i1)),label)
        i0=i1+2
      enddo
      f(n)=fpval(adjustl(list(i0:)),label)
c
      return
      end
c#######################################################################
      subroutine load_list_of_ints (s,label,n,j)
c
c-----------------------------------------------------------------------
c
c ****** Read N integer values from character string S into
c ****** array J(N).  The values in S may be either space or
c ****** comma separated.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: s
      character(*) :: label
      integer :: n
      integer, dimension(n) :: j
      intent(in) :: s,label,n
      intent(out) :: j
c
c-----------------------------------------------------------------------
c
      integer :: i,i0,i1
      character :: delimiter
      character(512) :: list
c
c-----------------------------------------------------------------------
c
      integer, external :: intval
c
c-----------------------------------------------------------------------
c
c ****** Make a local copy of the string (removing leading spaces).
c
      list=adjustl(s)
c
c ****** If any commas are present, use a comma as the delimiter.
c ****** Otherwise, one or more spaces is used as a delimiter.
c ****** In this case, compress multiple spaces into a single space.
c
      if (index(list,',').ne.0) then
        delimiter=','
      else
        delimiter=' '
        call delete_repeated_char (list,' ')
      end if
c
c ****** Read the list of N numbers sequentially into J.
c
      i0=1
      do i=1,n-1
        i1=scan(list(i0:),delimiter)+i0-2
        j(i)=intval(adjustl(list(i0:i1)),label)
        i0=i1+2
      enddo
      j(n)=intval(adjustl(list(i0:)),label)
c
      return
      end
c#######################################################################
      subroutine delete_repeated_char (s,c)
c
c-----------------------------------------------------------------------
c
c ****** Transform repeated adjoining occurrences of character C
c ****** in string S into single occurrences of C.
c
c ****** The string S is overwritten by the modified string.
c
c ****** Trailing blanks in S are ignored.
c
c-----------------------------------------------------------------------
c
c ****** For example, suppose this routine is called with C='d' and
c ****** S='abcdddeefdhdd'.  On return, S will have the value
c ****** 'abcdeefdhd'.
c
c-----------------------------------------------------------------------
c
c ****** This routine uses the FORTRAN90 intrinsic SCAN.
c
c-----------------------------------------------------------------------
c
      character(*) :: s
      character :: c
      intent(in) :: c
      intent(inout) :: s
c
c-----------------------------------------------------------------------
c
      integer :: i,i0
c
c-----------------------------------------------------------------------
c
      i0=1
      do
        i=scan(trim(s(i0:)),c)
        if (i.eq.0) exit
        i0=i0+i
        do
          if (s(i0:i0).ne.c) exit
          s(i0:)=s(i0+1:)
        enddo
      enddo
c
      return
      end
