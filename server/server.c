/* LazyCat server.
 *
 * Copyright (C) 2012-2013 Artyom Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of LazyCat.
 * 
 * LazyCat is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * LazyCat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <pthread.h>
#include <syslog.h>
#include <pthread.h>
#include <errno.h>      /* errno */
#include <unistd.h>

#include <sys/stat.h>   /* mkdir() */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>     /* struct sockaddr_un */

#include <libguile.h>

#include "common.h"
#include "xsockets.h"
#include "db.h"
#include "sighandlers.h"
#include "scmapi.h"

#include "tcp-proxy.h"
#include "ssh-proxy.h"

/*
 * Types
 */

typedef void (*proxy_t)(void);

/*
 * Global definitions
 */

pid_t* proc_list  = NULL;
int    proc_count = 0;

/*
 * Prototypes of static functions
 */

static int start_proxy (proxy_t proxy, char* name);
static void scm_thread (void *closure, int argc, char **argv);
static void init_builtins_module (void* arg);

/*
 * Main function
 */

int
main (int argc, char* argv[])
{
  enum { SYSLOG_MSG_LEN = 50 };

  char syslog_msg[SYSLOG_MSG_LEN];

  proxy_t tcp_proxy = tcp_proxy_init;
  proxy_t ssh_proxy = ssh_proxy_init;

  struct db_rec rec;

  snprintf(syslog_msg, SYSLOG_MSG_LEN, "lazycat [server (PID=%d)]", getpid());

  openlog (syslog_msg, LOG_CONS, LOG_USER);
  syslog (LOG_INFO, "-------------------------------------------------------");
  SYSLOG_INFO ("Log is opened.");

  /* Register signal handlers */
  reg_sighandlers ();

  /* Load proxies */
  
  rec.type = DB_REC_PROXY;
  bzero (&rec, sizeof rec);

  rec.name = strdup (TCP_PROXY_NAME);
  rec.fd = start_proxy (tcp_proxy, rec.name);
  db_insert (&rec);

  free (rec.name);

  rec.name = strdup (SSH_PROXY_NAME);
  rec.fd = start_proxy (ssh_proxy, rec.name);
  db_insert (&rec);

  free (rec.name);

  /* Start Guile */

  scm_boot_guile (argc, argv, scm_thread, 0);

  /* Never reached. */
  return 0;
}

/*
 * Start the proxy process.
 *
 * This function opens a socket for communicating with this
 * proxy. Returns descriptor of the new proxy.
 */

static int
start_proxy (proxy_t proxy, char* name)
{
  proc_count++;
  
  if (proc_count == 1)
    {
      proc_list = (pid_t*) calloc (1, sizeof (pid_t));
    }
  else
    {
      size_t new_size = proc_count * sizeof (pid_t);
      proc_list = (pid_t*) realloc (proc_list, new_size);
    }
  
  proc_list[proc_count] = fork ();

  if (proc_list[proc_count] > 0)
    {
      /* Open socket for communicating with proxy */
      return connect_to_socket (name);
    }
  else if (proc_list[proc_count] == 0)
    {
      closelog ();
      /* Start proxy process */
      proxy ();
    }
  else
    {
      SYSLOG_ERROR ("An error occured during forking the process.");
      return -1;
    }
}

/*
 * This function registers API for Guile and starts Guile process. 
 */
static void
scm_thread (void* closure, int argc, char* argv[])
{
  SCM main;
  SCM args;
  SCM module;

  scm_c_define_module ("lazycat server builtins", init_builtins_module, NULL);

  scm_primitive_load_path ( scm_from_locale_string ("main.scm") );

  module = scm_c_resolve_module ("lazycat server scm main");
  main   = scm_c_module_lookup (module, "main");
  args   = scm_program_arguments ();

  scm_call_1 (scm_variable_ref (main), args);
}

/*
 * Declare the module which provides LazyCat API
 */
static void
init_builtins_module (void* arg)
{
#define DEFINE_PUBLIC(func_name, param_count, c_func_name)              \
    do {                                                                \
      scm_c_define_gsubr (func_name, param_count, 0, 0, c_func_name);   \
      scm_c_export (func_name, NULL);                                   \
    } while (0)

  DEFINE_PUBLIC ("lc-send-msg",      2, scm_send_msg);
  DEFINE_PUBLIC ("lc-add-host",      4, scm_add_host);
  DEFINE_PUBLIC ("lc-rem-host",      1, scm_rem_host);
  DEFINE_PUBLIC ("lc-update-host",   3, scm_update_host);
  DEFINE_PUBLIC ("lc-get-host-list", 0, scm_get_host_list);  

#undef DEFINE_PUBLIC
}
