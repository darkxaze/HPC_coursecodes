#include <omp.h> 
#include <stdio.h> 
#include <stdlib.h> 
  
int main(int argc, char* argv[]) 
{
 
omp_set_dynamic(1);
    #pragma omp parallel 
    { 
       int id = omp_get_thread_num();
       int nthread = omp_get_num_threads(); 
       printf("This thread id %d fron total %d threads \n",id,nthread);
    } 
} 
