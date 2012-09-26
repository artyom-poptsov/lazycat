/* Various useful functions which are used in the LazyCat.
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

#include <string.h>

/*
 * This function allocates memory and makes a copy of the string src.
 */
char*
calcpy (char** dest, const char* src)
{
  size_t src_len = strlen (src);
  *dest = (char*) calloc (src_len, sizeof (char));
  if (*dest == NULL)
    return NULL;
  return strncpy (*dest, src, src_len);
}

/*
 * This function allocates memory and makes a copy of the first N symbols
 * of the string src.
 */
char*
calncpy (char** dest, const size_t src_len, const char* src)
{
  if ((src_len <= 0) || (src_len > strlen (src)))
    return NULL;
  
  *dest = (char*) calloc (src_len, sizeof (char));
  if (*dest == NULL)
    return NULL;
  
  return strncpy (*dest, src, src_len);
}
