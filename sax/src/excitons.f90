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
#include "sax_version.h"
program excitons_program
!
use ptk_module
use iotk_module
use excitons_options_module
use tools_module
use num_module
use pw_module
use pw_bse_interf
use pw_bse_type
use num_la_parall_module
implicit none

integer                  :: rank,ierr,npe

character(256)           :: args(256)
integer                  :: nargs

type(sax_options)        :: options   ! Contiene le opzioni globali del codice
type(pw_struct)          :: struct    ! Contiene i dati di struttura (dim. cella) del sistema in analisi
type(pw_atoms)           :: atoms     ! Struttura atomica e pseudopotenziali
type(pw_symmlist)        :: symmlist  ! Contiene una lista delle simmetrie del sistema
type(pw_kmesh)           :: kmesh     ! Contiene la mesh per le funzioni d'onda
type(pw_kmesh)           :: qmesh     ! Contiene la mesh per gli elementi di dipolo
                                      ! (le differenze in kmesh)
type(pw_bse)             :: bse
type(pw_states)          :: states

integer                  :: log_unit, iostat

type(pw_w)               :: w         ! Contiene W
logical                  :: ionode
real                     :: states_cutoff
integer                  :: ib        ! temp index
character(256)           :: infile,logfile

logical,        allocatable :: field_switch(:,:)
type(pw_field), allocatable :: field(:,:)
type(pw_wfc),   pointer     :: wfc
integer :: itrans,ib1,ib2,ik,i,istate
complex, allocatable :: acvk(:)
real :: mod_acvk, old_acvk(1:4), sum
integer :: old_ib1(1:4), old_ib2(1:4), old_ik(1:4)
complex, allocatable :: field_tmp(:,:,:)
type(pw_field) :: exciton
real, allocatable :: grid(:,:)
integer :: i1,i2,i3
integer :: dim1, dim2, dim3
integer :: str2, str3
real :: supercell(3,3)
integer :: nmax

call ptk_init(verbosity=1)
call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)

ionode = .false.
if(rank==0) ionode = .true.

if(ionode) then
  call iotk_readcmdline(args,nargs)
  if(nargs/=2) ERROR(" ")
  infile  = args(1)
  logfile = args(2)
end if

call ptk_bcast(infile,0,ptk_comm_world)
call ptk_bcast(logfile,0,ptk_comm_world)

call tools_log_set_file(trim(logfile))
call tools_artwork_print(tools_log_unit(),"sax")
call tools_log("www.sax-project.org, version "//__SAX_VERSION)
call tools_log("Program excitons.x starts")
call tools_log("This is log from pe "//tools_char(rank,3)//" of "//tools_char(npe,3))

call tools_log("Writing here initial mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here initial PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Pasting input file "//trim(infile)//" ...")
call iotk_copyfile(dest_unit=tools_log_unit(),source=trim(infile))
call tools_log("... done")

call tools_log("Opening input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_open_read(unit=15,file=trim(infile))
call tools_log(" done")

call tools_log("Reading options ...",advance=.false.)
call excitons_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log(" done")

call tools_log("Reading structure ...",advance=.false.)
call pw_struct_readbcast(struct,15,0,ptk_comm_world,name="structure")
call tools_log(" done")

call pw_atoms_init(atoms,struct,4.0)
call tools_log("Reading atoms ...",advance=.false.)
call pw_atoms_readbcast(atoms,15,0,ptk_comm_world,fmt="iotk",name="atoms")
call tools_log(" done")

call tools_log("Symmetries are not implemented")
call pw_symmlist_init(symmlist,struct)
call pw_symmlist_only_identity(symmlist)

call pw_kmesh_init(kmesh,symmlist)
call pw_kmesh_init(qmesh,symmlist)

call tools_log("Reading kmesh ...",advance=.false.)
call pw_kmesh_readbcast(kmesh,15,0,ptk_comm_world,fmt="iotk",name="kmesh")
call pw_kmesh_set_dim(qmesh,kmesh%m,(/0.0,0.0,0.0/))
call tools_log(" done")

call tools_log("Closing input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_close_read(unit=15)
call tools_log(" done")

call tools_log("Writing here the kmesh for PWSCF code ...")
call pw_kmesh_write(kmesh,tools_log_unit(),"pwscf")
call tools_log("... done")

select case(options%calculation_kind)
case("WAVES")
call tools_log("Calculation kind is WAVES",advance=.true.)
call tools_log("Excitonic wave functions calculated and writen in xcrysden format",advance=.true.)
call tools_log("Initializing states ...",advance=.false.)
call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
call tools_log(" done")

select case(options%start_from)
case("DFT")
  call tools_log("Reading states from file 'states' ...",advance=.false.)
  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.)
  call pw_states_read(states,10,"states")
  if(ionode) call iotk_close_read(10)
  call tools_log(" done")
case("HF")
  call tools_log("Reading states from file 'HF_states'...",advance=.false.)
  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
  call pw_states_read(states,10,"HF_states")
  if(ionode) call iotk_close_read(10)
  call tools_log("done")
case("GW")
  call tools_log("Reading states from file 'GW_states'...",advance=.false.)
  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
  call pw_states_read(states,10,"GW_states")
  if(ionode) call iotk_close_read(10)
  call tools_log("done")
case DEFAULT
  ERROR("Wrong start_from")
end select

! calculating single particle wfc cutoff for coulomb

  states_cutoff=pw_states_get_cutoff(states)
  call tools_log("The cutoff for single particle wfc is: ", advance=.false.)
  write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
  call tools_log(" Ry")

! to avoid deadlock...

  if(modulo(options%nbandmax-options%nbandmin+1,npe)/=0) &
    ERROR("nb bands in states should be a multiple of nb proc")


  call tools_log("Initializing bse ...",advance=.false.)
  call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
       options%bse_spin,0,ptk_comm_world)
  call tools_log("done")
  call tools_log("Reading bse from file bse...",advance=.false.)
  if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
  call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world)
  if(ionode) call iotk_close_read(66)
  call tools_log(" done")


case("ANALYSIS")
  call tools_log("Calculation kind is ANALYSIS",advance=.true.)
  call tools_log("Main contribution of single particle states to excitons writen in txt format")

  call tools_log("Reading ntrans ...",advance=.false.)
  if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
  call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world,not_allocated=.true.)
  if(ionode) call iotk_close_read(66)

  call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
       options%bse_spin,0,ptk_comm_world,find_trans=.false.)
  call tools_log("done")
  call tools_log("Reading bse from file bse...",advance=.false.)
  if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
  call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world)
  if(ionode) call iotk_close_read(66)
  call tools_log(" done")

!  call pw_bse_transpose(bse,0,ptk_comm_world)
case("MINCHIA")
  call tools_log("Calculation kind is MINCHIA",advance=.true.)

  call tools_log("Reading ntrans ...",advance=.false.)
  if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
  call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world,not_allocated=.true.)
  if(ionode) call iotk_close_read(66)

  call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
       options%bse_spin,0,ptk_comm_world,find_trans=.false.)
  call tools_log("done")
  call tools_log("Reading bse from file bse...",advance=.false.)
  if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
  call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world)
  if(ionode) call iotk_close_read(66)
  call tools_log(" done")
!  call tools_log("Calculating transpose ...",advance=.false.)
!  call pw_bse_transpose(bse,0,ptk_comm_world)
!  call tools_log(" done")
  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)
  call pw_bse_transpose(bse,0,ptk_comm_world)
! Write of bse
   call tools_log("Writing eigenvalues and eigenstates on file bse ...",advance=.false.)
   if(bse%matrix%rank==bse%matrix%root) call iotk_open_write(200001,file=trim(options%outdir)//"bse")
   call pw_bse_write(bse,200001,"bse")
   if(bse%matrix%rank==bse%matrix%root) call iotk_close_write(200001)
   call tools_log("done")
case DEFAULT
  ERROR("Wrong calculation kind")
end select

!!
if(options%calculation_kind=="WAVES") then
    supercell=matmul(struct%a,transpose(real(states%kmesh%m)))
    call pw_struct_set(struct,supercell)
    do ik=1,states%kmesh%nkbz
      states%basis(ik)%k=matmul(real(states%kmesh%m),states%basis(ik)%k)
      states%basis(ik)%g=matmul(states%kmesh%m,states%basis(ik)%g)
      call pw_basis_set_g_extr(states%basis(ik))
    end do

    
    allocate(acvk(bse%ntrans))

    call pw_field_init(exciton,struct)
    call pw_field_set_dim(exciton,options%supergrid,k=(/0.0,0.0,0.0/),r0=(/0.0,0.0,0.0/))


    allocate(field(states%nbmin:states%nbmax,states%kmesh%nkbz))
    allocate(field_switch(states%nbmin:states%nbmax,states%kmesh%nkbz))
    call pw_field_init(field,struct)

    dim1=exciton%dim(1)
    dim2=exciton%dim(2)
    dim3=exciton%dim(3)


    allocate(field_tmp(1:dim1,1:dim2,1:dim3))


    do istate=options%nstatemin,options%nstatemax ! 1,10

      acvk(:)=0.0
      exciton%val=0.0
      field_tmp(:,:,:) = 0.0
      field_switch=.false.
      if(bse%matrix%where_line(1,istate)==bse%matrix%rank) then
         acvk(:)=bse%matrix%val(bse%matrix%where_line(2,istate),:)
      endif
      do itrans=1,bse%ntrans
        ib1=bse%ib1trans(itrans)
        ib2=bse%ib2trans(itrans)
        ik=bse%iktrans(itrans)
        if(.not.field_switch(ib1,ik)) then
!          write(0,*) "ib1",ib1,ik
          field_switch(ib1,ik)=.true.
          call pw_states_borrow_wfc(states,wfc,ib1,ik)
          call pw_field_set_dim(field(ib1,1),options%supergrid,k=(/0.0,0.0,0.0/),r0=(/0.5,0.5,0.5/))
!
!this for drawing the hole
!          call pw_field_set_dim(field(ib1,1),(/1,1,1/),k=(/0.0,0.0,0.0/),r0=options%r_hole)
!
          call pw_wfc2field(field(ib1,1),wfc)
          call pw_states_giveback_wfc(states,wfc)
        end if
        if(.not.field_switch(ib2,ik)) then
!          write(0,*) "ib2",ib2,ik
          field_switch(ib2,ik)=.true.
          call pw_states_borrow_wfc(states,wfc,ib2,ik)
          call pw_field_set_dim(field(ib2,1),(/1,1,1/),k=(/0.0,0.0,0.0/),r0=options%r_hole)
! this is for drawing the hole
!          call pw_field_set_dim(field(ib2,1),options%supergrid,k=(/0.0,0.0,0.0/),r0=(/0.5,0.5,0.5/))
!
          call pw_wfc2field(field(ib2,1),wfc)
          call pw_states_giveback_wfc(states,wfc)
        end if

        exciton%val=exciton%val+acvk(itrans)*field(ib2,1)%val(1)*conjg(field(ib1,1)%val)
! this is for drawing the hole at fixed electron position
!        exciton%val=exciton%val+acvk(itrans)*field(ib2,1)%val*conjg(field(ib1,1)%val(1))
!
!        write(0,*) "after excitons%val+..." 
      end do

!      write(0,*) "exciton val"
!      write(0,*) bse%matrix%rank, (exciton%val(ib1), ib1=1,100)

      call ptk_allreduce_inplace(exciton%val,ptk_sum,ptk_comm_world)
!      call ptk_barrier(ptk_comm_world)

      if(ionode) then
        open(10,file=trim(options%outdir)//"state"//trim(iotk_index(istate))//".xsf",form="formatted",status="unknown")
        open(11,file=trim(options%outdir)//"state"//trim(iotk_index(istate))//".txt",form="formatted",status="unknown")
        call pw_field_write(exciton,11)
         call xsf_struct(struct%a,atoms%natoms, atoms%positions, atoms%names, atoms%type_map, 10)
        write(0,*) "after xsf struc"
         str2 = exciton%str2
         str3 = exciton%str3
         do i3=1,exciton%dim(3)
           do i2=1,exciton%dim(2)
             do i1=1,exciton%dim(1)
               i  = (i1-1) + (i2-1)*str2 + (i3-1)*str3 + 1
               field_tmp(i1,i2,i3) = exciton%val(i)
             end do
           end do
         end do

!         write(0,*) "before grid 3d"

         call xsf_fast_datagrid_3d(field_tmp,exciton%dim(1),exciton%dim(2), &
            exciton%dim(3), exciton%str2, exciton%str3,1,struct%a,10)
        close(10)
        close(11)

      endif
   end do

   call pw_field_destroy(exciton)



   deallocate(acvk)
   deallocate(field_tmp)

   call pw_field_destroy(field)

   deallocate(field,field_switch)

   call pw_states_destroy(states)

elseif(options%calculation_kind=="ANALYSIS") then

   allocate(acvk(bse%ntrans))

   nmax = 4

   if(bse%ntrans<4) nmax = bse%ntrans
   if(bse%ntrans==1) ERROR("only 1 transition has been calculated ####")

     if(ionode) &
        open(10,file=trim(options%outdir)//"BSE_ANALYSIS",form="formatted",status="unknown")

    
   do istate=options%nstatemin,options%nstatemax ! 1,10
      acvk(:) = 0.0
      sum = 0.0
      old_acvk=0.0
      old_ib1=0
      old_ib2=0
      old_ik=0
      if(bse%matrix%where_line(1,istate)==bse%matrix%rank) then
        acvk(:)=bse%matrix%val(bse%matrix%where_line(2,istate),:)
      endif
      call ptk_allreduce_inplace(acvk,ptk_sum,ptk_comm_world)
      do itrans=1,bse%ntrans
      mod_acvk=real(acvk(itrans))**2+imag(acvk(itrans))**2
      sum = sum + mod_acvk
      enddo

      sum = SQRT(sum)
     
      write(0,*) "sum: ", sum

      mod_acvk = 0.0      

      do itrans=1,bse%ntrans
        ib1=bse%ib1trans(itrans)
        ib2=bse%ib2trans(itrans)
        ik=bse%iktrans(itrans)
        mod_acvk=real(acvk(itrans))**2+imag(acvk(itrans))**2
        sum = sum + mod_acvk
          if((old_acvk(1)<mod_acvk)) then
            old_acvk(1)=mod_acvk
            old_ib1(1)=ib1
            old_ib2(1)=ib2
            old_ik(1)=ik
          endif
      enddo

      do i=2,nmax
      do itrans=1,bse%ntrans
        ib1=bse%ib1trans(itrans)
        ib2=bse%ib2trans(itrans)
        ik=bse%iktrans(itrans)
        mod_acvk=real(acvk(itrans))**2+imag(acvk(itrans))**2
          if((old_acvk(i)<mod_acvk).and.(mod_acvk<(old_acvk(i-1)-0.0000001))) then
            old_acvk(i)=mod_acvk
            old_ib1(i)=ib1
            old_ib2(i)=ib2
            old_ik(i)=ik
          endif
      enddo
      enddo

!     if(ionode) then
!        open(10,file="analysis_state"//trim(iotk_index(istate))//".txt",form="formatted",status="unknown")
        write(0,*) "---------------------------------------------"
        write(10,*) "EXCITON number: ", istate
        write(10,'("energy: ",f5.3,2x,"Ry")') bse%matrix%eigenval(istate)
        write(10,*)"v    c    k     Weigt"
        do i=1,4
          write(10,"(i4,2x,i4,2x,i4,2x,f5.3)") old_ib2(i), old_ib1(i), old_ik(i), old_acvk(i)  
        enddo
        write(0,*) "--------------------------------------------"
!        close(10)
!     endif
   enddo
   close(10)
   deallocate(acvk)
endif
    call pw_bse_destroy(bse)

! Destruction of all general structures
call pw_atoms_destroy(atoms)
call pw_kmesh_destroy(qmesh)
call pw_kmesh_destroy(kmesh)
call pw_symmlist_destroy(symmlist)

call tools_log("Writing here final mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here final PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Program excitons ends")

call ptk_finalize

end program excitons_program
