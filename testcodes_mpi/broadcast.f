       implicit none
       integer:: myid,ierr,nproc
       double precision :: a1
       include 'mpif.h'

       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)

       a1=0.
       if(myid.eq.0)a1=1.
      
       write(*,*)a1,'before broadcast at process#',myid
       call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        
       call MPI_BCAST(a1,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      
       write(*,*)a1,'after broadcast at process# ',myid

       call MPI_FINALIZE(ierr)
       
       stop
       end











        
