      program Hello_World
      integer omp_get_thread_num  
  
!$omp parallel private(id)

      id = omp_get_thread_num()
      write(*,*)'Hello World from thread =',id

!$omp end parallel

      end

