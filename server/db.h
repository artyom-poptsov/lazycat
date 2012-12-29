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

enum STATUS {
  OFFLINE,
  ONLINE
};


enum {
  MAX_PROXY_NAME_LEN  = 20,
  MAX_ADDRESS_LEN     = 100,
  MAX_HOST_NAME_LEN   = 100,
  MAX_HOST_DESC_LEN   = 100
};

/********************************************************************************
 * Types
 */

struct Rec_host
{
  int         id;
  
  /* Information related to communicating with host */
  char        proxy_name[MAX_PROXY_NAME_LEN];
  char        address[MAX_ADDRESS_LEN];
  
  /* Brief information about host */
  char        name[MAX_HOST_NAME_LEN];
  char        description[MAX_HOST_DESC_LEN];
  
  /* Status of the host */
  enum STATUS status;
};

struct Rec_proxy
{
  char  name[MAX_PROXY_NAME_LEN];
  int   fd;
};

/********************************************************************************
 * Interface to DB
 */

/* This function is used for getting a list of IDs of all hosts in DB */
int db_get_hosts_list (int* count, int* list[]);
  
/* This function makes a new record in DB for host. */
int db_add_host (struct Rec_host* host);

/* This function removes a host from DB. */
int db_rem_host (const int id);

int db_update_host (const int id, const char* field, const char* value);

/* This function checks if host with given ID exists in DB. */
int db_check_host (const int id);

/* This function is used for getting a record from DB for host with given ID. */
int db_get_host_record (const int id, struct Rec_host* host);

/* This function is used for setting a new status for host with given ID. */
int db_set_host_status (int id, enum STATUS new_status);

/* This function makes new record in DB for protocol. */
int db_add_proxy(struct Rec_proxy* proxy);

/* This function remves protocol from DB. */
int db_rem_proxy(const int fd);

/* This function is used for getting information about protocol. */
int db_get_proxy_by_fd (const int fd, struct Rec_proxy* proxy);

int db_get_proxy_by_name (const char* name, struct Rec_proxy* proxy);

int db_get_proxies_list (int* count, int* list[]);

#endif	/* ifndef __DB_H__ */
