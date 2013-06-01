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

#include <libguile.h>


/* Start the LazyCat main procedure. */
static void
inner_main (void* closure, int argc, char** argv)
{
  SCM main;
  SCM args;
  SCM module;

  module = scm_c_resolve_module ("lazycat lazycat-daemon main");
  main   = scm_c_module_lookup (module, "main");
  args   = scm_program_arguments ();

  scm_call_1 (scm_variable_ref (main), args);
}


/* Entry point of the program */
int
main (int argc, char* argv[])
{
  setenv ("GUILE_LOAD_PATH", GUILE_LOAD_PATH, 1);
  scm_boot_guile (argc, argv, inner_main, 0);

  return 0;
}

/* server.c ends here */
