#include <omp.h> 
#include <stdio.h> 
#include <stdlib.h> 
  
int main(int argc, char* argv[]) 
{
int i,id,a; 
#pragma omp parallel for  private(i,id,a)
for (i=0;i<10;i++)
{
id=omp_get_thread_num();
a = i+1; // updating private a
printf("a is %d for i= %d from thread: %d\n",a,i,id); 
} // writing out private a by each thread
printf("after: a is %d\n",a);// writing private a
}                            //outside parallel loop
