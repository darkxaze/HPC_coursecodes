/*

This program will numerically compute the integral of

                  4/(1+x*x) 
				  
from 0 to 1.  The value of this integral is pi -- which 
is great since it gives us an easy way to check the answer.

The is the original sequential program.  It uses the timer
from the OpenMP runtime library

History: Written by Tim Mattson, 11/99.

*/
#include <stdio.h>
#include <time.h>
#include <omp.h>
#define number_threads 40
static long num_interval = 1000000000;
double deltax;
int main ()
{
	  int i,id;
	  double x, pi,sum[number_threads];

          double cpu_time,start_time;

	  deltax = 1.0/(double) num_interval;

          omp_set_num_threads(number_threads);        	 
	  start_time = omp_get_wtime();


#pragma omp parallel for  default(shared) private (id,x) //schedule(guided,1000) 
          
	  for (i=1;i<= num_interval; i++){
          id = omp_get_thread_num();
		  x = (i-0.5)*deltax;
		  sum[id] = sum[id]+  (4.0/(1.0+x*x))*deltax;
	  }


          pi=0.;
         for( i=0;i<number_threads;i++)
            pi += sum[i];


          cpu_time=omp_get_wtime();
          cpu_time=cpu_time-start_time;
	  printf("\n pi calculated using %ld steps is %lf in %lf seconds\n ",num_interval,pi,cpu_time);
}	  





