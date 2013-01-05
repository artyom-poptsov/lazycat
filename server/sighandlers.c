/* Signal handlers
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

#include <stdlib.h>
#include <stdint.h>
#include <signal.h>
#include <syslog.h>

#include "db.h"
#include "server.h"

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
  int32_t* list;
  size_t   list_size;

  struct db_rec proxy;

  int32_t retval;
  int32_t idx;
  int32_t status;

  int32_t type = DB_REC_PROXY;
  
  retval = db_query (DB_COL_TYPE, &type, &list_size, &list);
  if (retval < 0)
    {
      syslog(LOG_ERR, "Unable to get proxies list from DB.");
      exit (EXIT_FAILURE);
    }

  for (idx = 0; idx < list_size; ++idx)
    {
      if (db_get (list[idx], &proxy) != 0)
	{
	  free (list);
	  exit (EXIT_FAILURE);
	}

      close (proxy.fd);

      free (proxy.name);
    }

  for (idx = 0; idx < proc_count; ++idx)
    {
      (void) kill (proc_list[idx], SIGTERM);
      (void) waitpid (proc_list[idx], &status, 0);
    }

  free (list);
  closelog ();
}
