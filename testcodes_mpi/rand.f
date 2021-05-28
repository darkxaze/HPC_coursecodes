
        include 'mpif.h'


       call MPI_INIT(ierr)


       call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
       call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
       do i = 1, 10
        write(*,*) rand(0),myid
      end do

       call MPI_FINALIZE(ierr)
       
      stop
      end
