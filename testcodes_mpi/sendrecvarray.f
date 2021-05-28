       implicit none
       include 'mpif.h'
       integer:: myid,ierr,nproc,i
       double precision, dimension(1000):: sendbuf,recvbuf
       integer :: status(MPI_STATUS_SIZE)

       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       if(nproc.ne.2)then
       write (*,*) 'wrong no of processes'
       goto 11
       endif
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
       do i=1,1000
       sendbuf(i)=i/2.
       end do
   
       if(myid.eq.0)then
       call MPI_SEND(sendbuf,1000,MPI_DOUBLE_PRECISION,1,10,
     &                MPI_COMM_WORLD,ierr)
       else
       call MPI_RECV(recvbuf,1000,MPI_DOUBLE_PRECISION,0,10,
     &                MPI_COMM_WORLD,status,ierr)
   
       endif
       if(myid.eq.0)open(1,file='process1',status='unknown')
       if(myid.eq.1)open(1,file='process2',status='unknown')

       do i=1,1000
       write(1,*)recvbuf(i)
       end do
 
  11   call MPI_FINALIZE(ierr)
       
       stop
       end











        
