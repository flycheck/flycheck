int main(void)
{
   int a[100];

   #pragma omp parallel for
   for (int i = 0; i < 100; i++) {
     a[i]= 2 * i;
   }

   return 0;
}
