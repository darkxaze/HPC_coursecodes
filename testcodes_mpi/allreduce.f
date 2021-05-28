       implicit none
       integer:: myid,ierr,nproc
       real :: a1,a,pi
       include 'mpif.h'

       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)

       pi=4.0*atan(1.0)
       a1=cos(myid*pi/(nproc))

       call MPI_ALLREDUCE(a1,a,1,MPI_REAL,
     * MPI_MIN,MPI_COMM_WORLD,ierr)

       write(*,*)"a=",a,"a1=",a1,"proc id:",myid

       call MPI_FINALIZE(ierr)
       
       stop
       end









        
