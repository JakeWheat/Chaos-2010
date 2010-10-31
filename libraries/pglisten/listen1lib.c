#include "listen1lib.h"

void pl_disconnect(PGconn *conn)
{
  PQfinish(conn);
}

static void
exit_nicely(PGconn *conn)
{
  pl_disconnect(conn);
  exit(1);
}

PGconn *pl_connect(const char *conninfo)
{
  PGconn *conn;
  fprintf(stderr,"connectdb %s\n", conninfo);
  conn = PQconnectdb(conninfo);
  // Check to see that the backend connection was successfully made
  if (PQstatus(conn) != CONNECTION_OK)
    {
      fprintf(stderr, "Connection to database failed: %s",
              PQerrorMessage(conn));
      exit_nicely(conn);
    }
  return conn;
}

static char *concat_strings(char *a, char *b) {
  char *res;
  res = (char *)malloc((strlen(a) + strlen(b) + 1) * sizeof(char));
  strcpy(res,a);
  strcat(res + strlen(a), b);
  return res;
}

void pl_listen(PGconn *conn, char *not)
{
  PGresult   *res;
  char *s = concat_strings("listen ", not);
  fprintf(stderr, "running %s\n", s);
  res = PQexec(conn, s);
  free(s);
  if (PQresultStatus(res) != PGRES_COMMAND_OK)
    {
      fprintf(stderr, "LISTEN command failed: %s", PQerrorMessage(conn));
      PQclear(res);
      exit_nicely(conn);
    }

  //should PQclear PGresult whenever it is no longer needed to avoid memory leak
  PQclear(res);
}

void pl_unlisten(PGconn *conn, char *not)
{
  PGresult   *res;
  char *s = concat_strings("unlisten ", not);
  fprintf(stderr, "running %s\n", s);
  res = PQexec(conn, s);
  free(s);
  if (PQresultStatus(res) != PGRES_COMMAND_OK)
    {
      fprintf(stderr, "UNLISTEN command failed: %s", PQerrorMessage(conn));
      PQclear(res);
      exit_nicely(conn);
    }

  //should PQclear PGresult whenever it is no longer needed to avoid memory leaks
  PQclear(res);
}

char *pl_read(PGconn *conn) {
  // Sleep until something happens on the connection.  We use select(2)
  //to wait for input, but you could also use poll() or similar
  // facilities.
  int         sock;
  fd_set      input_mask;
  PGnotify   *notify;
  char *not_name;

  sock = PQsocket(conn);

  if (sock < 0) {
    fprintf(stderr, "shouldn't happen: sock < 0");
    exit_nicely(conn);
  }

  FD_ZERO(&input_mask);
  FD_SET(sock, &input_mask);

 try_again:
  if (select(sock + 1, &input_mask, NULL, NULL, NULL) < 0)
    {
      fprintf(stderr, "select() failed: %s\n", strerror(errno));
      exit_nicely(conn);
    }

  // Now check for input
  PQconsumeInput(conn);
  if ((notify = PQnotifies(conn)) != NULL)
    {
      fprintf(stderr,
              "ASYNC NOTIFY of '%s' received from backend pid %d\n",
              notify->relname, notify->be_pid);
      not_name = (char *)malloc((strlen(notify->relname) + 1) * sizeof(char));
      strcpy(not_name, notify->relname);
      PQfreemem(notify);
      return not_name;
    } else {
    goto try_again;
  }
}
