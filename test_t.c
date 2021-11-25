#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define BST (+1)
#define CCT (+8)

int main(void)
{
   struct tm tm;
   char buf[255];
   
   time_t rawtime;
   struct tm *info;
   time_t seconds;
   
   int ret;
   struct tm info2;
   char buffer[80];

   time_t start_t, end_t;
   double diff_t;
   
   printf("Starting of the program...\n");
   time(&start_t);
   printf("Sleeping for 5 seconds...\n");
   //sleep(5);
   
   memset(&tm, 0, sizeof(tm));
   strptime("2001-11-12 18:31:01", "%Y-%m-%d %H:%M:%S", &tm);
   strftime(buf, sizeof(buf), "%d %b %Y %H:%M", &tm);
   puts(buf);
   // exit(EXIT_SUCCESS);

   time( &rawtime );
   info = localtime( &rawtime );
   printf("Current local time and date: %s", asctime(info));

   /* Get GMT time */
   info = gmtime(&rawtime );
   
   printf("Current world clock:\n");
   printf("London : %2d:%02d\n", (info->tm_hour+BST)%24, info->tm_min);
   printf("China  : %2d:%02d\n", (info->tm_hour+CCT)%24, info->tm_min);   
   //return(0);

   seconds = time(NULL); //can be rawtime
   printf("Hours since January 1, 1970 = %ld\n", seconds/3600);



   info2.tm_year = 2001 - 1900;
   info2.tm_mon = 7 - 1;
   info2.tm_mday = 4;
   info2.tm_hour = 0;
   info2.tm_min = 0;
   info2.tm_sec = 1;
   info2.tm_isdst = -1;

   ret = mktime(&info2);
   /*
   if( ret == -1 ) {
      printf("Error: unable to make time using mktime\n");
   } else {
      strftime(buffer, sizeof(buffer), "%c", &info2 );
      printf(buffer);
   }
   */
   
   time(&end_t);
   diff_t = difftime(end_t, start_t);

   printf("Execution time = %f\n", diff_t);
   printf("Exiting of the program...\n");

   return(0);   
}
