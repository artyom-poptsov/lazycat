/* Signal handlers
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

#include <stdlib.h>
#include <signal.h>
#include <syslog.h>

#include "db.h"

typedef void (*sighandler_t)(int);

static void term_handler_fn (int signum);

int
reg_sighandlers(void)
{
  sighandler_t term_handler = &term_handler_fn;
  signal (SIGTERM, term_handler);
}

static void
term_handler_fn (int signum)
{
  extern pid_t* proc_list;
  extern int    proc_count;
  
  int count;
  int* proxies_list;
  int retval;
  int idx;
  int status;
  
  retval = db_get_proxies_list (&count, &proxies_list);
  if (retval < 0)
    {
      syslog(LOG_ERR, "Unable to get proxies list from DB.");
      exit (EXIT_FAILURE);
    }

  for (idx = 0; idx < count; ++idx)
    {
      close (proxies_list[idx]);
    }

  for (idx = 0; idx < proc_count; ++idx)
    {
      (void) kill (proc_list[idx], SIGTERM);
      (void) waitpid (proc_list[idx], &status, 0);
    }

  closelog ();
}
