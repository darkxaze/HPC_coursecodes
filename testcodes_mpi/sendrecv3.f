       implicit none
       include 'mpif.h'
       integer:: myid,ierr,nproc
       double precision :: sendbuf,recvbuf
       integer :: status(MPI_STATUS_SIZE)

       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       if(nproc.ne.2)then
       write (*,*) 'wrong no of processes'
       goto 11
       endif
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
       sendbuf=1.
       if(myid.eq.1)sendbuf=2.
       recvbuf=0.
   
       if(myid.eq.0)then
       call MPI_SSEND(sendbuf,1,MPI_DOUBLE_PRECISION,1,10,
     &                MPI_COMM_WORLD,ierr)
       call MPI_RECV(recvbuf,1,MPI_DOUBLE_PRECISION,1,10,
     &                MPI_COMM_WORLD,status,ierr)
       else
c       call MPI_SSEND(sendbuf,1,MPI_DOUBLE_PRECISION,0,10,
c     &                MPI_COMM_WORLD,ierr)
       call MPI_RECV(recvbuf,1,MPI_DOUBLE_PRECISION,0,10,
     &                MPI_COMM_WORLD,status,ierr)
       call MPI_SSEND(sendbuf,1,MPI_DOUBLE_PRECISION,0,10,
     &                MPI_COMM_WORLD,ierr)

       endif



       write(*,*)status,'myid',myid
       write(*,*)myid,'sendbuf=',sendbuf,'recvbuf=',recvbuf
 
  11   call MPI_FINALIZE(ierr)
       
       stop
       end











        
