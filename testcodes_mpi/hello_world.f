       implicit none
       integer:: myid,ierr,nproc
       include 'mpif.h'


       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
 
       write(*,*)'Hello world from process',myid, 'of',nproc
      
       call MPI_FINALIZE(ierr)
       stop
       end











        
