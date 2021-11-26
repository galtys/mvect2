#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mongoose.h"

enum error_code {
  RET_OK=0,
  PARSE_PARTIAL=1,
  PARSE_ERROR=2
};
struct Tm_ec {
  struct tm *p_tm;
  int e_code;
};
typedef struct Tm_ec TM_EC;

struct tm *new_tm_info() {
  struct tm *p_tm;
  p_tm = malloc( sizeof(struct tm) );
  return p_tm;
}
void *free_tm_info(struct tm *p_tm) {
  free (p_tm);
}

struct tm *read_gmtime(int raw) {

  struct tm *p_tm;
  time_t rawtime;
  rawtime = raw;
  p_tm = gmtime(&rawtime);
    
  return p_tm;
}

time_t read_time() {
   time_t rawtime;
   int ret=0;
   time( &rawtime );   
   ret = rawtime;
   return ret;//rawtime;
}


char *wrap_strftime( int maxsize, const char *p_format, const struct tm *p_tm )
{
  size_t ret_size;
  char *p_buf;
  char *p_ret;
  //  struct tm *p_tm;
  time_t rawtime;
       
  p_buf = malloc( sizeof(char)*maxsize );
  
  ret_size = strftime(p_buf, maxsize, p_format, p_tm);
  
  p_ret = malloc( sizeof(char)*(ret_size+1) );
  
  for (int i=0; i<ret_size ; i++) {
    *(p_ret + i)  = *(p_buf + i);
  }
  *(p_ret + ret_size) = '\0';
  
  free (p_buf);
  //free (p_tm);
  return p_ret;
  
}

const TM_EC *wrap_strptime(const char *p_buf, const char *p_format) {
  char *p_ret;
  struct tm *p_tm;
  TM_EC *p_tm_ec;
    
  p_tm = malloc( sizeof(struct tm) );
  p_tm_ec = malloc( sizeof(TM_EC) );
  memset(p_tm, 0, sizeof(struct tm) );
  memset(p_tm_ec, 0, sizeof(TM_EC)  );
  
  p_ret = strptime(p_buf,p_format, p_tm);
  p_tm_ec->p_tm = p_tm;
  if (p_ret==NULL) {
    p_tm_ec->e_code=PARSE_ERROR;
  } else if (*p_ret=='\0') {
    p_tm_ec->e_code=RET_OK;
  } else {
    p_tm_ec->e_code=PARSE_PARTIAL;
  };
  return p_tm_ec;
}     

int test_gcd(int a, int b)
{
    int temp;
    while (b != 0)
    {
        temp = a % b;
        a = b;
        b = temp;
    }
    return a;
}

/*
int gcd(int m, int n)
{
        int tmp;
        while(m) { tmp = m; m = n % m; n = tmp; }       
        return n;
}
*/

int test_lcm(int m, int n)
{
  return (m / test_gcd(m, n) * n);
}
