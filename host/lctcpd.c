/* Host process that executes command from server.
 *
 * This one is used by tcp-proxy.
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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#include <syslog.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "common.h"
#include "xsockets.h"

/*
 * Global definitions
 */

const char F_ERR_FILE[] = "/tmp/lazycat-stderr";
const char F_OUT_FILE[] = "/tmp/lazycat-stdout";

static FILE* f_stderr;
static FILE* f_stdout;

/*
 * External functions
 */

extern int xsend_msg (const int sfd, const char* data, size_t data_sz);
extern int xrecv_msg (const int sfd, char** buf, size_t* buf_sz);

/*
 * Static functions
 */

static int main_loop (int sfd_server);

static void reg_signal_handlers (void);
static void termination_handler (int signum);

/*
 * Main
 */

int
main (int argc, char* argv[])
{
  static const char SYSLOG_MSG[]  = "lctcpd";
  
  int port = 50001;
  int sfd_server;
  int pid;
  int retval;
  
  openlog (SYSLOG_MSG, LOG_CONS, LOG_DAEMON);
  syslog (LOG_INFO, "-------------------------------------------------------");
  syslog (LOG_INFO, "Log is opened.");

  pid = fork ();
  if (pid > 0)
    {
      return EXIT_SUCCESS;
    }
  else if (pid < 0)
    {
      syslog (LOG_ERR, "Unable to fork() process.");
      closelog ();
      return EXIT_FAILURE;
    }

  /*
   * Close standard file descriptors
   */
  
  close (0);
  close (1);
  close (2);

  /*
   * Create new session for daemon
   */

  retval = setsid ();
  if (retval < 0)
    {
      syslog (LOG_ERR, "Unable to become session leader.");
      closelog ();
      return EXIT_FAILURE;
    }

  /*
   * Assign stderr and stdout to files
   */
  
  f_stderr = freopen (F_ERR_FILE, "a+", stderr);
  f_stdout = freopen (F_OUT_FILE, "a+", stdout);
  chmod(F_ERR_FILE, S_IRUSR | S_IWUSR);
  chmod(F_OUT_FILE, S_IRUSR | S_IWUSR);

  /*
   * Open socket
   */

  sfd_server = open_inet_socket (port);
  if (sfd_server < 0)
    {
      syslog (LOG_ERR, "Unable to open socket.");
      closelog ();
      return EXIT_FAILURE;
    }

  main_loop (sfd_server);
}

/*
 * Main loop of the lctcpd
 */

static int
main_loop (int sfd_server)
{
  enum { BACKLOG = 1 };

  int sfd_client;

  size_t msg_size;
  char*  msg_buf;

  int retval;

  int pid;
  long output_begin, output_end;

  listen (sfd_server, BACKLOG);

  while (TRUE)
    {
      sfd_client = accept (sfd_server, NULL, NULL);
      if (sfd_client < 0)
        {
	  syslog (LOG_WARNING, "Error occured during accept() call.");
	  sleep (5);
	  continue;
        }
      
      syslog (LOG_DEBUG, "-i- New connection accepted.");

      retval = xrecv_msg (sfd_client, &msg_buf, &msg_size);

      syslog (LOG_DEBUG, "<-- msg = %s", msg_buf);

      /*
       * Execute received string as bash command
       */

      fseek (f_stdout, 0, SEEK_END);
      output_begin = ftell (f_stdout);
      syslog (LOG_DEBUG, "-i- output_begin = %ld", output_begin);
      
      /* Set timestamp */
      (void) system ("date \"+[%Y-%m-%d %H:%M]\"");

      pid = fork ();
      
     if (pid == 0)
	{
	  execl (getenv("SHELL"), "bash", "-c", msg_buf, (char*) NULL);
	}
     else if (pid < 0)
	{
	  syslog (LOG_WARNING, "-*- Error occured during forking the process.");
	  close (sfd_client);
	  free (msg_buf);
	  continue;
	}

     waitpid (pid, NULL, 0);
     
     fseek (f_stdout, 0, SEEK_END);
     output_end = ftell (f_stdout);
     syslog(LOG_DEBUG, "-i- output_end = %ld", output_end);
     
     msg_size = (size_t) (output_end - output_begin);
     syslog(LOG_DEBUG, "-i- msg_size = %d", msg_size);
     
     free (msg_buf);
     
     msg_buf = (char*) calloc (msg_size, sizeof (char));

     fseek (f_stdout, output_begin, SEEK_SET);

     retval = fread (msg_buf, sizeof (char), msg_size, f_stdout);
     if (retval < (int) msg_size)
       {
	 syslog(LOG_WARNING, "-*- Error occured during reading output.");
	 close (sfd_client);
	 free (msg_buf);
	 continue;
       }

     retval = xsend_msg (sfd_client, msg_buf, msg_size);
     if (retval < 0)
       {
	 syslog(LOG_WARNING, "-*- Error occured during sending the message.");
	 close (sfd_client);
	 free (msg_buf);
	 continue;
       }

     free (msg_buf);
     
     close (sfd_client);
    }
}

static void
reg_signal_handlers (void)
{
  signal (SIGINT,  termination_handler);
  signal (SIGTERM, termination_handler);
  signal (SIGHUP,  termination_handler);
}

static void
termination_handler (int signum)
{
  fclose (f_stdout);
  fclose (f_stderr);
  
  closelog ();
}

