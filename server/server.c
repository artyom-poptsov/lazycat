/* LazyCat server.
 *
 * Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

static const char SYSLOG_MSG[] = "lazycat [server]";

pid_t* proc_list  = NULL;
int    proc_count = 0;

/*
 * External functions
 */

/* Proxies */
extern void tcp_proxy_init (void);
extern void ssh_proxy_init (void);

extern void register_sighandlers (void);

extern int db_add_proxy(struct Rec_proxy* proxy);

extern SCM scm_send_msg (SCM dest, SCM msg);
extern SCM scm_add_host (SCM proxy_name,
			 SCM address,
			 SCM name,
			 SCM description);

extern SCM scm_update_host (SCM host_id,
			    SCM field,
			    SCM value);

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
  proxy_t tcp_proxy = tcp_proxy_init;
  proxy_t ssh_proxy = ssh_proxy_init;

  struct Rec_proxy proxy;
    
  openlog (SYSLOG_MSG, LOG_CONS, LOG_USER);
  syslog (LOG_INFO, "-------------------------------------------------------");
  SYSLOG_INFO ("Log is opened.");

  /* Register signal handlers */
  reg_sighandlers ();

  /* Load proxies */
  
  strcpy(proxy.name, "tcp-proxy");
  proxy.fd = start_proxy (tcp_proxy, proxy.name);
  db_add_proxy (&proxy);

  strcpy(proxy.name, "ssh-proxy");
  proxy.fd = start_proxy (ssh_proxy, proxy.name);
  db_add_proxy (&proxy);

  /* Start Guile */
  
  scm_boot_guile (argc, argv, scm_thread, 0);

  /* Never reached. */
  return 0;
}

/*
 * This function is used for starting the proxy process.
 *
 * It adds new proxy to DB and opens socket for communicating with this proxy.
 * The function returns descriptor of the new proxy.
 */

static int
start_proxy (proxy_t proxy, char* name)
{
  extern int    open_socket (const char* socket_name);
  extern int    connect_to_socket (const char* socket_name);
  
  extern pid_t* proc_list;
  extern int    proc_count;

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
  scm_c_define_module ("lazycat builtins", init_builtins_module, NULL);
  scm_shell (argc, argv);
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
