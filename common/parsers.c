/* Various string parsers.
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
#include <string.h>
#include <stdint.h>

#include "common.h"

/* This function is used to get a user name from the address given in
 * following format: 
 *   user@host:port
 */
int32_t
get_username_from_address (const char *str, char **user_name)
{
  char   *pos;
  size_t user_name_len;
  
  if ((str == NULL) || (user_name == NULL))
    return -1;

  pos = strchr (str, '@');
  if (pos == NULL)
    {
      user_name = NULL;
      return 0;
    }

  user_name_len = pos - str;

  if (calncpy (user_name, user_name_len, str) == NULL)
    return -1;

  return user_name_len;
}

/* This function is used to get a host name from the address given in
 * following format: 
 *   user@host:port
 */
int32_t
get_hostname_from_address (const char *str, char **host_name)
{
  char *pos_begin;
  char *pos_end;
  size_t host_name_len;

  if ((str == NULL) || (host_name == NULL))
    return -1;

  pos_begin = strchr (str, '@');
  if (pos_begin == NULL)
    pos_begin = (char *) str;

  pos_end = strchr (str, ':');
  if (pos_end == NULL)
    pos_end = strchr (str, '\0');

  host_name_len = pos_end - pos_begin;
  if (calncpy (host_name, host_name_len, str) == NULL)
    return -1;

  return host_name_len;
}

/* This function is used to get a host name from the address given in
 * following format: 
 *   user@host:port
 */
int32_t
get_port_from_address (const char *str, uint16_t *port)
{
  char *pos_begin;
  char *pos_end;
  
  if (str == NULL)
    return -1;
  
  pos_begin = strrchr (str, ':');
  if (pos_begin == NULL)
    return 0;

  /* Move pointer to the first number */
  ++pos_begin;

  sscanf (pos_begin, "%hu", (unsigned short *) port);
  
  return *port;
}
