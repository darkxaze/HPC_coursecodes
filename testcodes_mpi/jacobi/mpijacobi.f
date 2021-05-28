ccccc Program Jacobi iteration   Laplace equation in 2D
cccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  MPI based domain decomposition in x direction
      implicit none
          
cccc Declare arrays for A matrix, b, x and x1 (new value) column vectors cccc
      double precision, allocatable, dimension(:,:) :: a
      double precision, allocatable,dimension(:)::b,x,x1
      integer, allocatable, dimension (:,:):: ipoint
      integer:: nx,ny,npoint,i,j,ii,iie,iiw,iin,iis,ita
      
      !mpi variables
      integer :: nmax,nrem,nstart,nend,nproc,myid,ierr,ict      
      double precision :: xerrormax,a_x,xerror,x1_new,err_nodemax
      double precision, allocatable :: tsendl(:),tsendr(:),
     &                                  trecvl(:),trecvr(:)
      
       character*70 paraout ! for output in parallel env

       !mpi library
        include 'mpif.h'
        integer :: stt(MPI_STATUS_SIZE)
        

       !mpi init calls
       
        call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)


      !read write by master processor
       if(myid.eq.0)then
cccc  Ask the user about the number of divisions in x and y cccccccc
      write(*,*)'nx and ny'
cccc  Read dimension of A from user input ccccccccccccccccccccc
      read(*,*)nx,ny
      endif
          
       !let others know it 

        call MPI_BCAST(nx,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        call MPI_BCAST(ny,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

      !load balancing- domain partioring

       nmax=nx/nproc
       nrem=nx-nmax*nproc
       if(myid.lt.nrem)then
       nmax=nmax+1
       endif

       if(nrem.eq.0)then
       nstart=1+nx/nproc*myid
       else
       if(myid.lt.nrem)nstart=1+(nx/nproc+1)*myid
       if(myid.ge.nrem)nstart=1+nx/nproc*myid+nrem
       endif
       nend=nstart+nmax
       
       if (myid.ne.0)nmax=nmax+1 ! overlap in left side
       if (myid.ne.nproc-1)nmax=nmax+1 !overlap in right side
       
       
      
       
ccccc Allocate dimension of pointer array ccc
       !allocation for subdomains
      allocate (ipoint(nmax+1,ny+1))

cccc define total number of point and pointers ccccc !subdomains
      npoint=(nmax+1)*(ny+1)
      do i=1,nmax+1
        do j=1,ny+1
        ipoint(i,j)=i+(j-1)*(nmax+1)
        end do
      end do

cccc  Allocate dimension of A, b and x  ccccccccccccccccccccc
      !allocation for subdomains
      allocate(a(npoint,npoint),b(npoint),x(npoint),x1(npoint))

cccc  Allocate pointers for data transfer
      allocate(tsendr(ny),tsendl(ny),trecvr(ny),trecvl(ny))
       
 
cccc   Initialize A and b with all zero ! for subdomains
       do i=1,npoint
         do j=1,npoint
           a(i,j)=0.0
         end do
         b(i)=0.
       end do

cccc  Form matrix A (n x n) and vector b (n) ccccccccccccccccccc
! for subdomains
      do i=1,nmax+1
        do j=1,ny+1
        if(i.eq.1.or.i.eq.nmax+1.or.j.eq.1.or.j.eq.ny+1)then
          a(ipoint(i,j),ipoint(i,j))=1.0
          if(j.eq.1)b(ipoint(i,j))=1.0 !lower boundary of all subdomains
          if(myid.eq.0.and.i.eq.1)b(ipoint(i,j))=1.0 !left boundary      
        else
         ii=ipoint(i,j)
         iie=ipoint(i+1,j)
         iiw=ipoint(i-1,j)
         iin=ipoint(i,j+1)
         iis=ipoint(i,j-1)
         a(ii,ii)=-4.
         a(ii,iie)=1.
         a(ii,iiw)=1.
         a(ii,iin)=1.
         a(ii,iis)=1.
        endif
        end do
       end do

       write(*,*)'aa',myid

cccc  Initial guess for solution vector x cccccccccccccccccccccc
       x(:)=0.0
       x1(:)=0.0


cccc  ITERATION PROCCESS STARTS HERE  cccccccccccccccccccccccccccc

cccc  Initialize iteration counter ita ccccccccccccccccccccccccc
       ita=0

cccc  Continue the iteration loop from line number 71 cccccccccc
 71    ita=ita+1   

cccc  Predefine the maximum error as a small value  cccccccccccc  
       xerrormax=-1e16
       
! set the interdomain boundary values through data transfer

       if(myid.ne.0)then
       ict=1
       do j=2,ny
       ict=ict+1
       tsendl(ict)=x(ipoint(3,j))
       end do
       endif
       if(myid.ne.nproc-1)then
       ict=1
       do j=2,ny
       tsendr(ict)=x(ipoint(nmax-1,j))
       ict=ict+1
       end do
       endif


        if(mod(myid,2).eq.0)then
        if(myid.lt.nproc-1)
     $  call MPI_SEND(tsendr,ny-1,MPI_DOUBLE_PRECISION,myid+1,10,
     &     MPI_COMM_WORLD,ierr)
        if(myid.gt.0)
     $  call MPI_RECV(trecvl,ny-1,MPI_DOUBLE_PRECISION,myid-1,10,
     &     MPI_COMM_WORLD,stt, ierr)
        if(myid.gt.0)
     $  call MPI_SEND(tsendl,ny-1,MPI_DOUBLE_PRECISION,myid-1,20,
     &     MPI_COMM_WORLD,ierr)
        if(myid.lt.nproc-1)
     $  call MPI_RECV(trecvr,ny-1,MPI_DOUBLE_PRECISION,myid+1,20,
     &     MPI_COMM_WORLD,stt,ierr)
        else
        call MPI_RECV(trecvl,ny-1,MPI_DOUBLE_PRECISION,myid-1,10,
     &     MPI_COMM_WORLD,stt,ierr)
        if(myid.lt.nproc-1)
     $  call MPI_SEND(tsendr,ny-1,MPI_DOUBLE_PRECISION,myid+1,10,
     &     MPI_COMM_WORLD,ierr)
        if(myid.lt.nproc-1)
     $  call MPI_RECV(trecvr,ny-1,MPI_DOUBLE_PRECISION,myid+1,20,
     &     MPI_COMM_WORLD,stt,ierr)
        call MPI_SEND(tsendl,ny-1,MPI_DOUBLE_PRECISION,myid-1,20,
     &     MPI_COMM_WORLD,ierr)
        endif


       if(myid.ne.0)then
       ict=1
       do j=2,ny
       b(ipoint(1,j))=trecvl(ict)
       ict=ict+1
       end do
       endif
       if(myid.ne.nproc-1)then
       ict=1
       do j=2,ny
       b(ipoint(nmax-1,j))=trecvr(ict)
       ict=ict+1
       end do
       endif




cccc  Calculation for each individual equation ccccccccccccccccc

! for subdomain
      do i=1,npoint

cccc  Sum up the a(ij)*x(j) (i.ne.j); x(j) last iteration guess cccccccc
        a_x=0.0
        do j=1,npoint
          if(j.ne.i)a_x=a_x-a(i,j)*x(j)
        end do

cccc  Form b(j)-a(ij)x(j), x(new) ccccccccccccccccccccccccccccc
        x1(i)=(b(i)+a_x)/a(i,i)

cccc  Find error in x and maximum error among all equations ccccccccc
        xerror=abs(x1(i)-x(i))
        xerrormax=max(xerrormax,xerror)

      end do
cccc  End of iteration and new x update for individual equations cccccc

cccc  Update x with newer values cccccccccccccccccccccccccccccccccccc
      x(:)=x1(:)

!put barrier  
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(xerrormax,err_nodemax,1,MPI_DOUBLE_PRECISION,
     *  MPI_MAX,MPI_COMM_WORLD,ierr)


!calculate global error

cccc  Write max error at iteration number  ccccccccccccccccccccc
!write for a single processor
      if(myid.eq.0)then
      write(*,*)'Error=',xerrormax,'@iteration no.',ita
      endif
cccc  For max error .gt. epsi continue iteratioN TO 71  cccccccccccccccc
      if(err_nodemax.gt.1e-8)goto 71
!write from master
      write(*,*) 'convergence' 
cccc  Open output file ccccccccccccccccccccccccccccccccccccccccc
!!! How to work on it?
      open(11,file='soln.dat',status='unknown')

cccc  Write x vector ccccccccccccccccccccccccccccccccccccccccccc
c      do i=1,npoint
c      write(11,*)'x_',i,x(i)
c      end do

cccc  Close all files cccccccccccccccccccccccccccccccccccccccccc
c      close(1)
c      close(2)
c      close(11)
       call MPI_FINALIZE (ierr)
      stop
      end

