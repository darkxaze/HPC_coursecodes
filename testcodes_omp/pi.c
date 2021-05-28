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
static long num_interval = 1000000000;
double deltax;
int main ()
{
	  int i;
	  double x, pi = 0.0;
          clock_t start, end;
          double cpu_time;

	  deltax = 1.0/(double) num_interval;

        	 
	  //start_time = omp_get_wtime();
   
          start=clock();
	  for (i=1;i<= num_interval; i++){
		  x = (i-0.5)*deltax;
		  pi = pi + (4.0/(1.0+x*x))*deltax;

	  }

          end= clock();
          cpu_time=((double) (end - start)) / CLOCKS_PER_SEC;
	  printf("\n pi calculated using %ld steps is %lf in %lf seconds\n ",num_interval,pi,cpu_time);
}	  





