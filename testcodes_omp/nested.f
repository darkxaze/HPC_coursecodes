      program nested
      integer omp_get_thread_num  
      call omp_set_nested(1) 
!$omp parallel private(id)
      id = omp_get_thread_num()
      write(*,*)'Hello World from thread =',id
!$omp parallel private(id)
      id=omp_get_thread_num()
      write(*,*)'hi all from thread=',id
!$omp end parallel
!$omp end parallel
      end

