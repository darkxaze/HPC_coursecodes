      implicit none
      integer ::   ndarts, nscore, n,ierr,myid,nprocs,i
      double precision ::	r
      double precision :: x_coord, x_sqr, y_coord, y_sqr, pi,pisum,piav

      include 'mpif.h'

      call MPI_INIT(ierr)
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, nprocs, ierr )
      nscore = 0
C     "throw darts at the board"
      piav=0.
      do 20 i=1,5
       ndarts =100000
       nscore=0
      do 10 n = 1, ndarts
C     generate random numbers for x and y coordinates
        call random_number(r)
        if(r.gt.0)r=r-(myid)*0.1
        if(r.le.0)r=r+(myid)*0.1
        x_coord = (2.0 * r) - 1.0
	x_sqr = x_coord * x_coord
        call random_number(r)
        if(r.gt.0)r=r-(myid)*0.1
        if(r.le.0)r=r+(myid)*0.1
        y_coord = (2.0 * r) - 1.0
	y_sqr = y_coord * y_coord
C       if dart lands in circle, increment score
	if ((x_sqr + y_sqr) .le. 1.0) then
	  nscore = nscore + 1
        endif
 10   continue

c      get pi 
        pi=4.*nscore/ndarts

c      get sum pi
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)

        call MPI_ALLREDUCE( pi,pisum, 1, MPI_DOUBLE_PRECISION, 
     &                   MPI_SUM,MPI_COMM_WORLD, ierr )

         
C     average pi
      if (myid.eq.0)then
      pi=pisum/nprocs
      piav = ((piav*(i-1)) + pi) / i
      write(*,*)'after', i*ndarts*nprocs, 'throws, pi=',piav
      endif
      pisum=0.

  20  end do
        if(myid.eq.0)write(*,*)'Real value of PI: 3.1415926535897'

      call MPI_FINALIZE(ierr)
      stop
      end

