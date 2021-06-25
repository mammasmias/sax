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
module pw_atoms_module
use pw_common_module
use pw_struct_module
use pw_pseudo_module
implicit none
private
public :: pw_atoms, &
          pw_atoms_readbcast,pw_atoms_bcast, &
           pw_atoms_init,pw_atoms_destroy,pw_atoms_read,pw_atoms_write, &
          pw_atoms_ewald

!@ END MANUAL


!@ MANUAL
type pw_atoms
  type (pw_struct), pointer :: struct
  integer                   :: natoms,ntypes
  type (pw_pseudo), pointer :: pseudo(:)
  character(100),    pointer :: names(:)
  real,             pointer :: positions(:,:)
  integer,          pointer :: type_map(:)
  real                      :: cutoff_vloc
end type pw_atoms
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_atoms_init(atoms,struct,cutoff_vloc)
  type (pw_atoms),          intent(out) ::atoms
  type (pw_struct), target, intent(in)  :: struct
  real, intent(in) :: cutoff_vloc
!@ END MANUAL
  atoms%struct => struct
  atoms%natoms = 0
  atoms%ntypes = 0
  atoms%cutoff_vloc = cutoff_vloc
  allocate(atoms%pseudo(0))
  allocate(atoms%names(0))
  allocate(atoms%positions(0,0))
  call pw_allocate(atoms%positions)
  allocate(atoms%type_map(0))
  call pw_allocate(atoms%type_map)
end subroutine pw_atoms_init

!@ MANUAL
subroutine pw_atoms_destroy(atoms)
  type (pw_atoms), intent(inout) :: atoms
!@ END MANUAL
  call pw_pseudo_destroy(atoms%pseudo)
  deallocate(atoms%pseudo)
  deallocate(atoms%names)
  call pw_deallocate(atoms%positions)
  deallocate(atoms%positions)
  call pw_deallocate(atoms%type_map)
  deallocate(atoms%type_map)
end subroutine pw_atoms_destroy

!@ MANUAL
subroutine pw_atoms_set_dim(atoms,natoms,ntypes)
  type (pw_atoms), intent(inout) :: atoms
  integer,         intent(in)    :: natoms,ntypes
!@ END MANUAL
  type (pw_struct), pointer :: struct
  struct => atoms%struct
  call pw_atoms_destroy(atoms)
  atoms%struct => struct
  atoms%natoms = natoms
  atoms%ntypes = ntypes
  allocate(atoms%pseudo(ntypes))
  allocate(atoms%names(ntypes))
  atoms%names = " "
  allocate(atoms%positions(3,natoms))
  call pw_allocate(atoms%positions)
  allocate(atoms%type_map(natoms))
  call pw_allocate(atoms%type_map)
  call pw_pseudo_init(atoms%pseudo)
  atoms%positions = 0.0
  atoms%type_map = 0
end subroutine pw_atoms_set_dim

subroutine pw_atoms_readbcast(atoms,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (pw_atoms), intent(inout) :: atoms
  integer,         intent(in)    :: unit
  character(*),    intent(in)    :: fmt,name
  integer,         intent(in)    :: root
  type (ptk_comm), intent(in)    :: comm
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call pw_atoms_read(atoms,unit,fmt,name)
  call pw_atoms_bcast(atoms,root,comm)
end subroutine pw_atoms_readbcast

!@ MANUAL
subroutine pw_atoms_read(atoms,unit,fmt,name)
  use tools_module
  use num_module
  use iotk_module
  type (pw_atoms), intent(inout) :: atoms
  integer,         intent(in)    :: unit
  character(*),    intent(in)    :: fmt,name
!@ END MANUAL
  integer :: natoms,ntypes,itype,iatom,unit_pseudo,iostat
  real :: pos(3),cutoff,alat, tmp(3,3)
  character(150) :: dummy
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: units
  select case(fmt)
  case ("iotk")
    call iotk_scan_begin(unit,trim(name))
    call iotk_scan_begin(unit,"types",attr)
    call iotk_scan_attr (attr,"ntypes",ntypes)
    call iotk_scan_end  (unit,"types")
    call iotk_scan_begin(unit,"positions",attr)
    call iotk_scan_attr (attr,"natoms",natoms)
    call iotk_scan_attr (attr,"units",units)
    call iotk_scan_attr (attr,"alat",alat,default=0.0) ! alat is optional
    call iotk_scan_end  (unit,"positions")
    call pw_atoms_set_dim(atoms,natoms,ntypes)
    call iotk_scan_begin(unit,"types")
    do itype=1,ntypes
      read(unit,*,iostat=iostat) atoms%names(itype),dummy,cutoff
      IOCHECK(unit,iostat)
      call iotk_free_unit(unit_pseudo)
      open(unit_pseudo,file=trim(dummy),status="old",iostat=iostat)
      IOCHECK(unit,iostat)
      call pw_pseudo_read(atoms%pseudo(itype),unit_pseudo,"upf")
      close(unit_pseudo)
      call pw_pseudo_set_table(atoms%pseudo(itype),cutoff)
    end do
    call iotk_scan_end(unit,"types")
    call iotk_scan_begin(unit,"positions")
    do iatom=1,natoms
      read(unit,*,iostat=iostat) dummy,pos
      IOCHECK(unit,iostat)
      do itype=1,ntypes
        if(trim(dummy)==trim(atoms%names(itype))) exit
      end do
      if(itype > ntypes ) ERROR("")
      select case(tools_uppercase(trim(units)))
      case ("CRYSTAL")
        pos = pos
      case ("BOHR")
        tmp = atoms%struct%b
        pos = num_matmul(transpose(tmp),pos) / num_2pi
      case ("ANGSTROM")
        tmp = atoms%struct%b
        pos = num_matmul(transpose(tmp),pos/num_au2a) / num_2pi
      case ("ALAT")
        if(alat==0.0) ERROR(" alat is missing")
        tmp = atoms%struct%b
        pos = num_matmul(transpose(tmp),pos*alat) / num_2pi
      case default
        ERROR(tools_uppercase(trim(units)))
      end select
      atoms%positions(:,iatom) = pos
      atoms%type_map(iatom) = itype
    end do
    call iotk_scan_end  (unit,"positions")
    call iotk_scan_end  (unit,trim(name))
  case default
    ERROR("")
  end select
end subroutine pw_atoms_read

!@ MANUAL
subroutine pw_atoms_write(atoms,unit,fmt)
  type (pw_atoms), intent(in) :: atoms
  integer,         intent(in) :: unit
  character(*),    intent(in) :: fmt
!@ END MANUAL
  integer :: iostat,i
  select case (fmt)
  case ("txt")
    write(unit,"(a)",iostat=iostat) "%PW_ATOMS"
    IOCHECK(unit,iostat)
    write(unit,"(a)",iostat=iostat) "NOTE: pseudos are not written"
    IOCHECK(unit,iostat)
    write(unit,"(a,i6)",iostat=iostat) "natoms ",atoms%natoms
    IOCHECK(unit,iostat)
    write(unit,"(a,i6)",iostat=iostat) "ntypes ",atoms%ntypes
    IOCHECK(unit,iostat)
    write(unit,"(a)",   iostat=iostat) "names"
    IOCHECK(unit,iostat)
    do i=1,atoms%ntypes
      write(unit,"(a)", iostat=iostat) atoms%names(i)
      IOCHECK(unit,iostat)
    end do
    write(unit,"(a)",   iostat=iostat) "type position"
    IOCHECK(unit,iostat)
    do i=1,atoms%natoms
      write(unit,"(i6,3f15.9)") atoms%type_map(i),atoms%positions(:,i)
      IOCHECK(unit,iostat)
    end do
    write(unit,"(a)",iostat=iostat) "%END_PW_ATOMS"
    IOCHECK(unit,iostat)
  case default
    ERROR(fmt)
  end select
end subroutine pw_atoms_write

subroutine pw_atoms_bcast(atoms,root,comm)
  use ptk_module, only : ptk_comm_rank,ptk_comm_size,ptk_bcast,ptk_comm
  type (pw_atoms), intent(inout) :: atoms
  integer,         intent(in)    :: root
  type (ptk_comm), intent(in)    :: comm
  integer :: natoms,ntypes,itype
  integer :: rank,ips,size
  real :: cutoff
  character(len=len(atoms%names)) :: str
  call ptk_comm_rank(comm,rank)
  call ptk_comm_size(comm,size)
  if(root >= size .or. root<0) ERROR("")
  if(rank==root) then
    natoms = atoms%natoms
    ntypes = atoms%ntypes
  end if
  call ptk_bcast(natoms,root,comm)
  call ptk_bcast(ntypes,root,comm)
  if(rank/=root) then
    call pw_atoms_set_dim(atoms,natoms=natoms,ntypes=ntypes)
  end if
  call pw_pseudo_bcast(atoms%pseudo,root,comm)
  do ips=1,ntypes
    cutoff = atoms%pseudo(ips)%cutoff
    call ptk_bcast(cutoff,root,comm)
    if(rank/=root) then
      call pw_pseudo_set_table(atoms%pseudo(ips),cutoff)
    end if
  end do
  do itype=1,ntypes
    str = atoms%names(itype)
    call ptk_bcast(str,root,comm)
    atoms%names(itype) = str
  end do
  call ptk_bcast(atoms%positions,root,comm)
  call ptk_bcast(atoms%type_map,root,comm)
end subroutine pw_atoms_bcast

function pw_atoms_ewald(atoms) result(ewald)
  use num_module
  use pw_coulomb_module
  use pw_basis_module
  use pw_wfc_module
  type (pw_atoms), intent(in) :: atoms
  real                        :: ewald
  real,allocatable :: z(:)
  integer :: iatom1,iatom2
  type (pw_basis)  :: basis
  type (pw_wfc)    :: vl
  type (pw_coulomb):: coulomb_l,coulomb_s
  real :: sigma,cutoff,vlr0,vsq0,rmax,dr(3),moddr
  integer :: r1max,r2max,r3max,ir1,ir2,ir3,ipw
  allocate(z(atoms%natoms))
  do iatom1 = 1 , atoms%natoms
    z(iatom1) = atoms%pseudo(atoms%type_map(iatom1))%z
  end do
  ewald = 0.0
  sigma = exp(1.0/3.0 * log(atoms%struct%b_omega))
  call pw_coulomb_init(coulomb_l,atoms%struct%b,iqsigma=+1,qsigma=sigma)
  call pw_coulomb_init(coulomb_s,atoms%struct%b,iqsigma=-1,qsigma=sigma)
  cutoff = (16.0*sigma)**2 ! cutoff su G^2
  rmax   = 20.0 / sigma ! cutoff su R
  vsq0  = pw_coulomb_total_integral(coulomb_s)
  vlr0  = pw_coulomb_contact_value(coulomb_l)
  r1max = rmax * sqrt(sum(atoms%struct%b(:,1)**2)) / num_2pi
  r2max = rmax * sqrt(sum(atoms%struct%b(:,2)**2)) / num_2pi
  r3max = rmax * sqrt(sum(atoms%struct%b(:,3)**2)) / num_2pi
  ewald = ewald - sum(z)**2 * vsq0 / atoms%struct%a_omega
  do iatom1 = 1 , atoms%natoms
    ewald = ewald - z(iatom1)**2 * vlr0
  end do
  call pw_basis_init(basis,atoms%struct)
  call pw_basis_create(basis,(/0.0,0.0,0.0/),cutoff)
  call pw_wfc_init(vl,basis)
  call pw_coulomb_get(3,coulomb_l,vl)
  do iatom1 = 1 , atoms%natoms
    do iatom2 = 1 , atoms%natoms
      dr = atoms%positions(:,iatom1) - atoms%positions(:,iatom2)
      do ipw=1,basis%npw
        ewald = ewald + z(iatom1) * z(iatom2) * &
                vl%val(ipw) * exp(-num_2pi_i * dot_product(real(basis%g(:,ipw)),dr))  / atoms%struct%a_omega
      end do
    end do
  end do
  call pw_wfc_destroy(vl)
  call pw_basis_destroy(basis)
  do iatom1 = 1 , atoms%natoms
    do iatom2 = 1 , atoms%natoms
      do ir1=-r1max,+r1max
      do ir2=-r2max,+r2max
      do ir3=-r3max,+r3max
        dr = atoms%positions(:,iatom1) + (/ir1,ir2,ir3/) - atoms%positions(:,iatom2)
        moddr = sqrt(sum(num_matmul(atoms%struct%a,dr)**2))
        if(moddr < 0.0000001) cycle
        ewald = ewald + z(iatom1) * z(iatom2) * 2.0/moddr * num_erfc(sigma * moddr / num_sqrt2)
      end do
      end do
      end do
    end do
  end do

  ewald = ewald * 0.5

  
  call pw_coulomb_destroy(coulomb_l)
  call pw_coulomb_destroy(coulomb_s)
  deallocate(z)
end function pw_atoms_ewald

end module pw_atoms_module

