program epsilon_from_oscil

character(len=10) :: dummy
integer :: istate, idummy
real*8 :: oscil(1:3)
real*8 :: omegamax, deltaomega
integer :: nomega
real*8 :: eta
complex*16, allocatable :: epsilon(:,:)

read(*,*) nstate
read(*,*) eta
read(*,*) omegamax
read(*,*) nomega

deltaomega = omegamax/real(nomega)

allocate(epsilon(1:3,0:nomega))
epsilon(:,:) = 0.D0

open(10,file="rpaexc_oscillators",status="old")

do istate =1,nstate
read(10,*) dummy, idummy, energy, oscil(1), oscil(2), oscil(3)
do iomega=0,nomega
omega=deltaomega*iomega
epsilon(1,iomega) = epsilon(1,iomega)+oscil(1)* &
  (1.D0/(omega-energy+(0.0,1.0)*eta)+1.D0/(omega+energy-(0.0,1.0)*eta))

epsilon(2,iomega) = epsilon(2,iomega)+oscil(2)* &
  (1.D0/(omega-energy+(0.0,1.0)*eta)+1.D0/(omega+energy-(0.0,1.0)*eta))

epsilon(3,iomega) = epsilon(3,iomega)+oscil(3)* &
  (1.D0/(omega-energy+(0.0,1.0)*eta)+1.D0/(omega+energy-(0.0,1.0)*eta))

enddo
enddo

close(10)
epsilon(:,:) = epsilon(:,:)*(-1.D0)

do iomega=0,nomega
omega=deltaomega*iomega
write(*,*) omega, imag(epsilon(1,iomega)), imag(epsilon(2,iomega)), imag(epsilon(3,iomega))
enddo
 
deallocate(epsilon)


end program epsilon_from_oscil
