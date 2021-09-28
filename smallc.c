#include <stdio.h>
#include "mongoose.h"

typedef struct mg_mgr MG_MGR;
typedef struct mg_connection MG_CONNECTION;
typedef struct mg_http_serve_opts MG_HTTP_SERVE_OPTS;
typedef struct mg_str MG_STR; // 16
typedef struct mg_http_header MG_HTTP_HEADER;  //32
typedef struct mg_http_message MG_HTTP_MESSAGE;

typedef struct mg_ws_message MG_WS_MESSAGE;


/*   UTILS  */

char *get_pvoid_NULL() {
  return NULL;
}

unsigned int get_size_t(unsigned int y) {
  unsigned int x;
  x = sizeof(size_t);
  return x;
  }
unsigned int get_size_int(unsigned int y) {
  unsigned int x;
  MG_HTTP_MESSAGE ocas;
  x = sizeof(ocas);
  return x;
}

/* String utils */

char *get_pchar_NULL() {
  return NULL;
}

void* mkString(char* str) {
    return (void*)str;
}

char* getString(void *p) {
    return (char*)p;
}

int isNullString(void* str) {
    return str == NULL;
}

int get_len(char* msg) {
  return strlen(msg);
}


/* MG_MGR */

MG_MGR *get_and_malloc__mg_mgr() {
  MG_MGR *p_mgr;
  p_mgr = malloc( sizeof(MG_MGR) );
  return p_mgr;
}

int is_null_mg_mgr(MG_MGR *p_mgr) {
  if (p_mgr == NULL) {
    return 1;
  } else {
    return 0;
  }
}

int free_mg_mgr(MG_MGR *p_mgr) {
  if (is_null_mg_mgr(p_mgr)==1) {
      return 0;
  } else {
    free ( (void *) p_mgr );    
    return 0;
  }
}

/* MG_HTTP_MESSAGE */

MG_HTTP_SERVE_OPTS *get_and_malloc__mg_http_serve_opts(char *r_dir) {
  MG_HTTP_SERVE_OPTS *p_opts;
  p_opts = malloc( sizeof(MG_HTTP_SERVE_OPTS) );
  memset(p_opts, 0, sizeof(MG_HTTP_SERVE_OPTS));  
  p_opts->root_dir = r_dir;
  return p_opts;
}

unsigned int get_MG_MAX_HTTP_HEADERS() {
  return MG_MAX_HTTP_HEADERS;
}

char *charFromMG_STR( MG_STR mg) {
  char *p_str;
  size_t N;
  
  N = mg.len;  
  
  p_str = (char *) malloc ( (N+1) * sizeof(char) );
  
  for (int i=0; i<N ; i++) {
    *(p_str + i)  = *(mg.ptr + i);
  }
  *(p_str + N) = '\0';
  return p_str;
}

char *print_hm(MG_HTTP_MESSAGE *p_hm) {

  char *p_m;
  p_m = charFromMG_STR( p_hm -> method);
  //MG_STR x;
  
  printf("-----------------------\n");
  printf("mX %d \n",(unsigned int) (p_hm->method.len) );
  printf("mX %s \n", p_m );

  printf("uriX %d \n",(unsigned int) (p_hm->uri.len) );
  printf("uriX %s \n", charFromMG_STR(p_hm->uri) );
  
  printf("-----------------------\n");
  return p_m;
}

char *get_hm_method(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> method);
  return p_m;
}
char *get_hm_uri(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> uri);
  return p_m;
}
char *get_hm_query(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> query);
  return p_m;
}
char *get_hm_proto(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> proto);
  return p_m;
}
char *get_hm_body(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> body);
  return p_m;
}

char *get_hm_message(MG_HTTP_MESSAGE *p_hm) {
  char *p_m;
  p_m = charFromMG_STR( p_hm -> message);
  return p_m;
}

char *get_hm_ith_header_name(MG_HTTP_MESSAGE *p_hm, unsigned int i) {
  //MG_HTTP_HEADER *p_h;
  char *p_m;  
  p_m = charFromMG_STR(  (p_hm -> headers)[i].name    );
  //p_m = charFromMG_STR( p_hm -> );
  return p_m;
}
char *get_hm_ith_header_value(MG_HTTP_MESSAGE *p_hm, unsigned int i) {
  //MG_HTTP_HEADER *p_h;
  char *p_m;  
  p_m = charFromMG_STR(  (p_hm -> headers)[i].value    );
  //p_m = charFromMG_STR( p_hm -> );
  return p_m;
}

MG_HTTP_MESSAGE *ev_to_http_message(void *ev_data) {
  MG_HTTP_MESSAGE *p_hm;
  p_hm = (MG_HTTP_MESSAGE *) ev_data;

  //print_hm(p_hm);
  return p_hm;
}

/* MG_WS_MESSAGE */

MG_WS_MESSAGE *ev_to_ws_message(void *ev_data) {
  MG_WS_MESSAGE *p_ws = (MG_WS_MESSAGE *) ev_data;
  return p_ws;
}


/* static dir fn handler */

void fn_http_handler(MG_CONNECTION *c, int ev, void *ev_data, void *fn_data) {
  struct mg_http_serve_opts opts = {.root_dir = "."};   // Serve local dir
  if (ev == MG_EV_HTTP_MSG) mg_http_serve_dir(c, ev_data, &opts);
}
