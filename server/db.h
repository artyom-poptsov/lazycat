/* Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __DB_H__
#define __DB_H__

#include <stdint.h>

/* Wildcard character that can be used in queries to the DB. */
#define DB_WILDCARD 0

/* Status of a proxy or a host */
enum STATUS {
  OFFLINE = 1,
  ONLINE
};

/* Type of a record */
enum DB_REC_TYPE {
  DB_REC_PROXY = 1,
  DB_REC_HOST
};

/* Column numbers in the DB. */
enum DB_COL {
  DB_COL_ID = 1,
  DB_COL_TYPE,      
  DB_COL_PROXY_NAME,
  DB_COL_NAME,
  DB_COL_ADDR,
  DB_COL_DESC,
  DB_COL_STAT
};

/* Single record in the DB. */
struct db_rec
{
  enum DB_REC_TYPE type;
  int32_t fd;

  char *proxy_name;
  char *name;
  char *addr;
  char *desc;

  enum STATUS stat;
};


/*
 * Interface to the DB
 */

/* Add a new record REC to the DB. */
int32_t db_insert (struct db_rec *rec);

/* Remove a record with the given ID from the DB */
int32_t db_delete (const int32_t id);

/* Replace current record with id ID with a new one. */
int32_t db_update (const int32_t id, struct db_rec *rec);

/* Check that a host with the id ID is exists in DB.  This function
   works faster than db_get () because it does no copying of host'
   data. */
int32_t db_check (const int32_t id);

/* Get a record with the given ID. */
int32_t db_get (const int32_t id, struct db_rec *rec);

/* Query DB for information.  The function searches a value VALUE in
   column COL and stores IDs of matched records in a newly allocated
   LIST of size SIZE.  List should be freed after usage.
   Return 0 on success, -1 on failure. */
int32_t db_query (enum DB_COL col, void *value, size_t *size, int32_t **list);

#endif	/* ifndef __DB_H__ */
