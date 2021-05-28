ccccc Program Jacobi iteration
cccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc
      implicit none
      
cccc Declare arrays for A matrix, b, x and x1 (new value) column vectors cccc
      double precision, allocatable, dimension(:,:) :: a	
      double precision, allocatable,dimension(:)::b,x,x1
      integer, allocatable, dimension (:,:):: ipoint
      integer:: nx,ny,npoint,i,j,ii,iie,iiw,iin,iis,ita      
      double precision :: xerrormax,a_x,xerror,x1_new
 
cccc  Ask the user about the number of divisions in x and y cccccccc
      write(*,*)'nx and ny'
cccc  Read dimension of A from user input ccccccccccccccccccccc
      read(*,*)nx,ny
      
ccccc Allocate dimension of pointer array ccc
      allocate (ipoint(nx+1,ny+1))

cccc define total number of point and pointers ccccc
      npoint=(nx+1)*(ny+1)
      do i=1,nx+1
        do j=1,ny+1
        ipoint(i,j)=i+(j-1)*(nx+1)
        end do
      end do

cccc  Allocate dimension of A, b and x  ccccccccccccccccccccc
      allocate(a(npoint,npoint),b(npoint),x(npoint),x1(npoint))
      
cccc   Initialize with all zero
       do i=1,npoint
         do j=1,npoint
           a(i,j)=0.0
         end do
         b(i)=0.
         x(i)=0.
         x1(i)=0.
       end do

cccc  Form matrix A (n x n) and vector b (n) ccccccccccccccccccc
      do i=1,nx+1
        do j=1,ny+1
        if(i.eq.1.or.i.eq.nx+1.or.j.eq.1.or.j.eq.ny+1)then
          a(ipoint(i,j),ipoint(i,j))=1.0
          if(i.eq.1.or.j.eq.1)b(ipoint(i,j))=1.0       
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

cccc  Initial guess for solution vector x cccccccccccccccccccccc
       x(:)=0.0


cccc  ITERATION PROCCESS STARTS HERE  cccccccccccccccccccccccccccc

cccc  Initialize iteration counter ita ccccccccccccccccccccccccc
       ita=0

cccc  Continue the iteration loop from line number 71 cccccccccc
 71    ita=ita+1   

cccc  Predefine the maximum error as a small value  cccccccccccc  
       xerrormax=-1e16


cccc  Calculation for each individual equation ccccccccccccccccc
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

cccc  Write max error at iteration number  ccccccccccccccccccccc
      write(*,*)'Error=',xerrormax,'@iteration no.',ita

cccc  For max error .gt. epsi continue iteratioN TO 71  cccccccccccccccc
      if(xerrormax.gt.1e-8)goto 71
      write(*,*) 'convergence' 
cccc  Open output file ccccccccccccccccccccccccccccccccccccccccc
      open(11,file='soln.dat',status='unknown')

cccc  Write x vector ccccccccccccccccccccccccccccccccccccccccccc
      do i=1,npoint
      write(11,*)'x_',i,x(i)
      end do

cccc  Close all files cccccccccccccccccccccccccccccccccccccccccc
      close(1)
      close(2)
      close(11)
      stop
      end

