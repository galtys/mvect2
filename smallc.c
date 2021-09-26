#include <stdio.h>
#include "mongoose.h"

typedef struct mg_mgr MG_MGR;
typedef struct mg_connection MG_CONNECTION;

MG_MGR *get_and_malloc_mg_mgr() {
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

void fn_http_handler(MG_CONNECTION *c, int ev, void *ev_data, void *fn_data) {
  struct mg_http_serve_opts opts = {.root_dir = "."};   // Serve local dir
  if (ev == MG_EV_HTTP_MSG) mg_http_serve_dir(c, ev_data, &opts);
}


int add(int x, int y) {
    return x+y;
}

int get_len(char* msg) {
  return strlen(msg);
}

int addWithMessage(char* msg, int x, int y) {
    printf("%s: %d + %d = %d\n", msg, x, y, x+y);
    return x+y;
}



 
/*  
#include "mongoose.h"
...

...

int main() {
  ...

  struct mg_mgr mgr;                                
  mg_mgr_init(&mgr);
  mg_http_listen(&mgr, "0.0.0.0:8000", fn, NULL);     // Create listening connection
  for (;;) mg_mgr_poll(&mgr, 1000);                   // Block forever
}
*/
