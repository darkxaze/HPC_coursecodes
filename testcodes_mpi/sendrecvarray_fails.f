       implicit none
       include 'mpif.h'
       integer:: myid,ierr,nproc,i
       double precision, dimension(10000) :: sendbuf,recvbuf
       integer :: status(MPI_STATUS_SIZE)

       call MPI_INIT(ierr)

       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       if(nproc.ne.2)then
       write (*,*) 'wrong no of processes'
       goto 11
       endif
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
       do i=1,10000
       sendbuf(i)=1.
       recvbuf(i)=0.
       end do
   
       if(myid.eq.0)then
       call MPI_SEND(sendbuf,10000,MPI_DOUBLE_PRECISION,1,10,
     &                MPI_COMM_WORLD,ierr)
       call MPI_RECV(recvbuf,10000,MPI_DOUBLE_PRECISION,1,10,
     &                MPI_COMM_WORLD,status,ierr)
       else
       call MPI_SEND(sendbuf,10000,MPI_DOUBLE_PRECISION,0,10,
     &                MPI_COMM_WORLD,ierr)
       call MPI_RECV(recvbuf,10000,MPI_DOUBLE_PRECISION,0,10,
     &                MPI_COMM_WORLD,status,ierr)

       endif



       write(*,*)status,'myid',myid
 
  11   call MPI_FINALIZE(ierr)
       
       stop
       end











        
