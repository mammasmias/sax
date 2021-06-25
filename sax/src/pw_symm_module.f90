! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#include "tools_error.h"

!@ MANUAL
module pw_symm_module
use pw_struct_module
implicit none
private
public :: pw_symm, &
          operator(*), &
          pw_symm_write, &
          pw_symm_init, &
          pw_symm_destroy, &
          assignment(=), &
          pw_symm_set_struct, &
          pw_symm_check, &
          pw_symm_set, &
          pw_symm_read, &
          pw_symm_bcast, &
          operator(==), &
          pw_symm_identity, pw_symm_time_reversal
!@ END MANUAL

!@ MANUAL
type pw_symm
  type(pw_struct), pointer :: struct
  logical                  :: conjg
  integer                  :: rrot(3,3)
  real                     :: rshift(3)
  integer                  :: grot(3,3)
! struct    :: puntatore alla struttura
! conjg     :: flag che indica se fare il coniugato in spazio reale
! rrot(:,:) :: matrice di rotazione valida per vettori espressi sulla base struct%a(:,:)
! rshift(:) :: vettore di traslazione della simmetria
! grot(:,:) :: matrice di rotazione valida per vettori espressi sulla base struct%b(:,:)
end type pw_symm
!@ END MANUAL

integer, parameter :: identity(3,3) = reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/))

!@ MANUAL
interface operator(*)
  module procedure pw_symm_compose
end interface
interface operator(==)
  module procedure pw_symm_is_equal
end interface
interface assignment(=)
  module procedure pw_symm_copy
end interface
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_symm_init(symm,struct)
! subroutine che inizializza un oggetto symm
  type(pw_symm),                     intent(out) :: symm
  type(pw_struct), target, optional, intent(in)  :: struct
! symm   :: oggetto da inizializzare
! struct :: struttura a cui deve puntare
!@ END MANUAL
  symm%conjg  = .false.
  symm%rrot   = identity
  symm%rshift = 0.0
  symm%grot   = identity
  nullify(symm%struct)
  if(present(struct)) call pw_symm_set_struct(symm,struct)
end subroutine pw_symm_init

!@ MANUAL
subroutine pw_symm_destroy(symm)
! subroutine che distrugge
  type(pw_symm), intent(inout) :: symm
! symm :: oggetto da distruggere
! NB nonostante pw_symm non contenga oggetti allocati dinamicamente
!    ho preferito usare la procedura di inizializzazione e distruzione
!    solita per annullare il puntatore a struct
!@ END MANUAL
  nullify(symm%struct)
end subroutine pw_symm_destroy

!@ MANUAL
subroutine pw_symm_time_reversal(symm)
  type(pw_symm), intent(inout) :: symm
!@ END MANUAL
  call pw_symm_set(symm,  conjg  = .true.,   &
                                rrot   = identity, &
                                rshift = (/0.0,0.0,0.0/),  &
                                check  = .true.)
end subroutine pw_symm_time_reversal

!@ MANUAL
subroutine pw_symm_set_struct(symm,struct)
! Subroutine per associare una struttura alla simmetria
  type(pw_symm),           intent(inout) :: symm
  type(pw_struct), target, intent(in)    :: struct
! symm   :: oggetto da impostare
! struct :: struttura da associare
!@ END MANUAL
  symm%struct => struct
end subroutine pw_symm_set_struct

!@ MANUAL
subroutine pw_symm_copy(new_symm,old_symm)
! Subroutine per copiare una simmetria
! interfacciata da operator(=)
  type(pw_symm), intent(inout) :: new_symm
  type(pw_symm), intent(in)    :: old_symm
!@ END MANUAL
  if(.not.associated(old_symm%struct)) ERROR("")
  call pw_symm_set_struct(new_symm,old_symm%struct)
  new_symm%conjg  = old_symm%conjg
  new_symm%rrot   = old_symm%rrot
  new_symm%rshift = old_symm%rshift
  new_symm%grot   = old_symm%grot
end subroutine pw_symm_copy

!@ MANUAL
function pw_symm_is_equal(symm1,symm2)
! Subroutine per confrontare due simmetrie
! interfacciata da operator(==)
  logical                   :: pw_symm_is_equal
  type(pw_symm), intent(in) :: symm1,symm2
!@ END MANUAL
  if(.not.associated(symm1%struct)) ERROR("")
  if(.not.associated(symm2%struct)) ERROR("")
  pw_symm_is_equal = associated(symm1%struct,symm2%struct) .and. &
                     ((symm1%conjg.and.symm2%conjg) .or. (.not.(symm1%conjg).and..not.(symm2%conjg))) .and. &
                     all(symm1%rrot==symm2%rrot) .and. &
                     all(symm1%grot==symm2%grot) .and. &
                     all(abs(symm1%rshift-symm2%rshift)<1e-6)
end function pw_symm_is_equal

!@ MANUAL
subroutine pw_symm_set(symm,conjg,rrot,rshift,check)
  use num_module
! Subroutine per impostare uno o piu' parametri della simmetria
  type(pw_symm),     intent(inout) :: symm
  logical,           intent(in)    :: conjg
  integer,           intent(in)    :: rrot(3,3)
  real,              intent(in)    :: rshift(3)
  logical, optional, intent(in)    :: check
! symm              :: oggetto da impostare
! conjg,rrot,rshift :: impostano i vari parametri della simmetria
!                      NB impostando rrot la routine calcola anche grot
! check             :: se check==.true. controlla la consistenza delle simmetrie rispetto al reticolo
!                      NB se check e' assente si intende check=.true. (default)
!@ END MANUAL
  logical :: check_local
  real :: ata(3,3),btb(3,3),grot(3,3),a(3,3),b(3,3),tmp(3,3)
  if(.not.associated(symm%struct)) ERROR("")
  check_local = .true. ! valore di dafault
  if(present(check)) check_local=check
  symm%conjg = conjg
  symm%rrot=rrot
  a = symm%struct%a
  b = symm%struct%b
  ata=num_matmul(transpose(a),a)
  btb=num_matmul(transpose(b),b)
  tmp = num_matmul(ata,real(rrot))
  grot = num_matmul(tmp,btb)/(num_2pi)**2
  symm%grot = nint(grot)
  if(check_local .and. .not. pw_symm_check(symm))then
    call pw_symm_write(symm,0)
    ERROR("")
  end if
! commented because it works in a strange way
!  if(any(abs(grot-nint(grot))>1e-2)) then
!    write(0,*) symm%struct%a
!    write(0,*) symm%struct%b
!    write(0,*) rrot
!    write(0,*) grot
!    ERROR("")
!  end if
  symm%rshift=rshift
  if(symm%conjg) symm%grot = -symm%grot
end subroutine pw_symm_set

!@ MANUAL
function pw_symm_check(symm)
  use num_module
! Subroutine per testare la consistenza delle simmetrie rispetto al reticolo
! funziona verificando che [rrot^t a^t a rrot == a^t a]
  logical                   :: pw_symm_check
  type(pw_symm), intent(in) :: symm
! se e' consistente     -> pw_symm_check=.true.
! se non e' consistente -> pw_symm_check=.false
!@ END MANUAL
  real :: ata(3,3),should_be_zero(3,3),rtatar(3,3),ar(3,3), tmp(3,3)
  if(.not.associated(symm%struct)) ERROR("")
  tmp = symm%struct%a
  ata=num_matmul(transpose(tmp),tmp)
  ar = num_matmul(symm%struct%a,real(symm%rrot))
!  rtatar=matmul(matmul(transpose(symm%rrot),ata),symm%rrot)
  rtatar = num_matmul(transpose(ar),ar)
  should_be_zero = rtatar-ata
  pw_symm_check = .not. any(abs(should_be_zero)>1e-6)
if(.not.pw_symm_check) then
  write(0,*) "should_be_zero",should_be_zero
  write(0,*) "a"
  write(0,"(3f15.9)") symm%struct%a(1,:)
  write(0,"(3f15.9)") symm%struct%a(2,:)
  write(0,"(3f15.9)") symm%struct%a(3,:)
  write(0,*) "r"
  write(0,"(3i5,f15.9)") symm%rrot(1,:)
  write(0,"(3i5,f15.9)") symm%rrot(2,:)
  write(0,"(3i5,f15.9)") symm%rrot(3,:)
  write(0,*) "ar"
  write(0,"(3f15.9)") ar(1,:)
  write(0,"(3f15.9)") ar(2,:)
  write(0,"(3f15.9)") ar(3,:)
  write(0,*) "rrot^t a^t a rrot"
  write(0,"(3f15.9)") rtatar(1,:)
  write(0,"(3f15.9)") rtatar(2,:)
  write(0,"(3f15.9)") rtatar(3,:)
end if
end function pw_symm_check

!@ MANUAL
function pw_symm_compose(symm1,symm2) result(res)
  use num_module
! Subroutine per comporre due simmetrie
! NB nella simmetria risultante viene copiata anche la struttura di symm1 e symm2
!    (che ovviamente deve essere consistente)
  type(pw_symm)             :: res
  type(pw_symm), intent(in) :: symm1,symm2
!@ END MANUAL
  if(.not.associated(symm1%struct)) ERROR("")
  if(.not.associated(symm2%struct)) ERROR("")
  if(.not.associated(symm1%struct,symm2%struct)) ERROR("")
  call pw_symm_set_struct(res,symm1%struct)
  res%rrot = num_matmul(symm1%rrot,symm2%rrot)
  res%rshift = num_matmul(real(symm1%rrot),symm2%rshift) + symm1%rshift
  res%grot = num_matmul(symm1%grot,symm2%grot)
  res%conjg = (symm1%conjg .and. .not. symm2%conjg) .or. &
              (.not. symm1%conjg .and. symm2%conjg)
end function pw_symm_compose

!@ MANUAL
subroutine pw_symm_write(symm,unit,fmt)
  use pw_common_module
! Subroutine per scrivere su file una simmetria
  type(pw_symm),          intent(in) :: symm
  integer,                intent(in) :: unit
  character(*), optional, intent(in) :: fmt
! symm :: oggetto da scrivere
! unit :: unita'
! fmt  :: formato
!           "txt" testo standard
!           "log" formato di log
!           "bin" formato binario
!@ END MANUAL
  integer :: iostat
  character(50) :: fmt_local
  if(present(fmt))      fmt_local=fmt
  if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
  if(.not.associated(symm%struct)) ERROR("")
  select case(fmt_local)
  case("txt")
    write(unit,"(a)",        iostat=iostat) "%PW_SYMM"
    if(iostat/=0) ERROR("")
    write(unit,"(l5)",       iostat=iostat) symm%conjg
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(1,:),symm%rshift(1)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(2,:),symm%rshift(2)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(3,:),symm%rshift(3)
    if(iostat/=0) ERROR("")
    write(unit,"(a)",        iostat=iostat) "%END_PW_SYMM"
    if(iostat/=0) ERROR("")
  case("log")
    write(unit,"(a)",        iostat=iostat) "*** Symmetry ***"
    if(iostat/=0) ERROR("")
    write(unit,"(a,l5)",     iostat=iostat) "Real space conjugation:",symm%conjg
    if(iostat/=0) ERROR("")
    write(unit,"(a)",        iostat=iostat) "Real space rotational matrix (in crystal vectors base)"
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(1,:)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(2,:)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%rrot(3,:)
    if(iostat/=0) ERROR("")
    write(unit,"(a)",        iostat=iostat) "Real space translational vector (in crystal vectors base)"
    if(iostat/=0) ERROR("")
    write(unit,"(3f15.9)",   iostat=iostat) symm%rshift(:)
    if(iostat/=0) ERROR("")
    write(unit,"(a)",        iostat=iostat) "Reciprocal space rotational matrix (in crystal vectors base)"
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%grot(1,:)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%grot(2,:)
    if(iostat/=0) ERROR("")
    write(unit,"(3i5,f15.9)",iostat=iostat) symm%grot(3,:)
    if(iostat/=0) ERROR("")
    write(unit,"(a)",        iostat=iostat) "*** End Symmetry ***"
    if(iostat/=0) ERROR("")
  case("bin")
    write(unit,iostat=iostat) symm%conjg,symm%rrot,symm%rshift
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_symm_write

!@ MANUAL
subroutine pw_symm_read(symm,unit,fmt)
  use pw_common_module
! Subroutine per leggere da file una simmetria
  type(pw_symm),          intent(out) :: symm
  integer,                intent(in)  :: unit
  character(*), optional, intent(in)  :: fmt
! symm :: oggetto da scrivere
! unit :: unita'
! fmt  :: formato
!           "txt" testo standard
!           "bin" formato binario
!@ END MANUAL
  integer :: iostat
  character(50) :: fmt_local
  logical :: conjg
  integer :: rrot(3,3)
  real    :: rshift(3)
  character(12) :: dummy
  if(present(fmt))      fmt_local=fmt
  if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
  if(.not.associated(symm%struct)) ERROR("")
  select case(fmt_local)
  case("txt")
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%PW_SYMM") ERROR("")
    read(unit,*,iostat=iostat) conjg
    if(iostat/=0) ERROR("")
    read(unit,*,iostat=iostat) rrot(1,1:3),rshift(1)
    if(iostat/=0) ERROR("")
    read(unit,*,iostat=iostat) rrot(2,1:3),rshift(2)
    if(iostat/=0) ERROR("")
    read(unit,*,iostat=iostat) rrot(3,1:3),rshift(3)
    if(iostat/=0) ERROR("")
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%END_PW_SYMM") ERROR(dummy)
  case("bin")
    write(unit,iostat=iostat) conjg,rrot,rshift
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
  call pw_symm_set(symm,conjg,rrot,rshift)
end subroutine pw_symm_read

!@ MANUAL
subroutine pw_symm_bcast(symm,root,comm)
! Subroutine per fare il broadcast di una simmetria
  use ptk_module, only : ptk_bcast,ptk_comm
  type(pw_symm), intent(inout) :: symm
  integer,       intent(in)    :: root
  type(ptk_comm),intent(in)    :: comm
! symm :: oggetto da distribuire
! root :: processo root
! comm :: communicator mpi
!@ END MANUAL
  if(.not.associated(symm%struct)) ERROR("")
  call ptk_bcast(symm%conjg,root,comm)
  call ptk_bcast(symm%rrot,root,comm)
  call ptk_bcast(symm%rshift,root,comm)
  call ptk_bcast(symm%grot,root,comm)
end subroutine pw_symm_bcast

!@ MANUAL
subroutine pw_symm_identity(symm)
  type(pw_symm), intent(inout) :: symm
!@ END MANUAL
  call pw_symm_set(symm,  conjg  = .false.,  &
                                rrot   = identity, &
                                rshift = (/0.0,0.0,0.0/),      &
                                check  = .true.)
end subroutine pw_symm_identity

end module pw_symm_module
