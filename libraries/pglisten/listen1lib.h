#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include "libpq-fe.h"

void pl_disconnect(PGconn *conn);
PGconn *pl_connect(const char *conninfo);
void pl_listen(PGconn *conn, char *not);
void pl_unlisten(PGconn *conn, char *not);
char *pl_read(PGconn *conn);
