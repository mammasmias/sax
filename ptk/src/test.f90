program test
use ptk_module
implicit none
integer :: i,rank

call ptk_init(verbosity=1)

call ptk_comm_rank(comm=ptk_comm_world,rank=rank)

i = rank

write(0,*) "BEFORE",rank,i

call ptk_bcast(i,root=0,comm=ptk_comm_world)

write(0,*) "AFTER",rank,i

call ptk_finalize

end program test
