/* Homebrew DB for storing information about hosts.
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
#include <stdio.h>
#include <string.h>
#include <syslog.h>

#include "db.h"

const char DB_NAME[] = "db";

/*
 *   Types
 * ==============================================================================
 */

struct Node_host
{
  struct Rec_host   rec;
  struct Node_host* prev;
  struct Node_host* next;
};

struct Node_proxy 
{
  struct Rec_proxy   rec;
  struct Node_proxy* prev;
  struct Node_proxy* next;  
};


/*
 *   Global definitions
 * ==============================================================================
 */

static int hosts_count;
static int proxies_count;
static struct Node_host*  hosts_linked_list;
static struct Node_proxy* proxies_linked_list;


/*
 *   Prototypes
 * ==============================================================================
 */

static struct Node_host*
get_free_node_for_host(struct Node_host* head);

static struct Node_host*
get_host_by_id(const int id, struct Node_host* head);

static struct Node_proxy*
get_free_node_for_proxy(struct Node_proxy* head);

static struct Node_proxy*
get_proxy_by_fd(const int fd, struct Node_proxy* head);

static struct Node_proxy*
get_proxy_by_name(const char* name, struct Node_proxy* head);


/*
 *   Interface to DB
 * ==============================================================================
 */

int
db_get_hosts_list (int* count, int* list[])
{
  extern int hosts_count;
  struct Node_host* node;
  int idx;

  if ((count == NULL) || (list == NULL))
    {
      return -1;
    }

  *list = (int*) calloc (hosts_count, sizeof (int));
  
  if (*list == NULL)
    {
      syslog (LOG_WARNING, "%s: Unable to allocate memory.", DB_NAME);
      return -1;
    }
  
  *count = hosts_count;

  for (node = hosts_linked_list, idx = 0;
       node && (idx < hosts_count);
       node = node->next, ++idx)
    {
      (*list)[idx] = node->rec.id;
    }
  
  return 0;
}

/* Functions for working with clients */

int
db_add_host (struct Rec_host* host)
{
  extern struct Node_host* hosts_linked_list;
  extern int               hosts_count;
  
  struct Node_host* new_node;
  
  if (hosts_linked_list == NULL)
    {
      /* If linked list is not exists, create the first node */
      hosts_linked_list =
	(struct Node_host*) malloc (sizeof (struct Node_host));
      hosts_linked_list->prev = NULL;
      hosts_linked_list->next = NULL;
      new_node = hosts_linked_list;
    }
  else
    {
      /* Add node to existing list */
      new_node = get_free_node_for_host (hosts_linked_list);
      if (new_node == NULL)
	{
	  syslog (LOG_WARNING, "%s: Unable to allocate memory "
		  "for new node in DB", DB_NAME);
	  return -1;
	}
    }
  
  new_node->rec.id = ++hosts_count;

  strncpy (new_node->rec.proxy_name, host->proxy_name,
	   (size_t) MAX_PROXY_NAME_LEN);
  
  strncpy (new_node->rec.address, host->address,
	   (size_t) MAX_ADDRESS_LEN);
  
  strncpy (new_node->rec.name, host->name,
	   (size_t) MAX_HOST_NAME_LEN);
  
  strncpy (new_node->rec.description, host->description,
	   (size_t) MAX_HOST_DESC_LEN);

  new_node->rec.status = host->status;
  new_node->next = NULL;
 
  return new_node->rec.id;
}

int
db_rem_host (int id)
{
  extern struct Node_host* hosts_linked_list;
  extern int               hosts_count;
  
  struct Node_host* node = get_host_by_id (id, hosts_linked_list);
  
  if (node == NULL)
    {
      syslog (LOG_WARNING, "%s: Node with id=%d isn't found.", DB_NAME, id);
      return -1;
    }
  
  if (node->prev)
    node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;

  free (node);
  
  if (node == hosts_linked_list)
    hosts_linked_list = NULL;
  
  --hosts_count;
  
  return 0;
}

int
db_check_host (const int id)
{
  extern struct Node_host* hosts_linked_list;
  return (get_host_by_id (id, hosts_linked_list) == NULL)? -1 : 0;
}

int
db_get_host_record (const int id, struct Rec_host* host)
{
  extern struct Node_host* hosts_linked_list;
  struct Node_host* node = get_host_by_id (id, hosts_linked_list);
  
  if (host == NULL || node == NULL)
    {
      return -1;
    }

  memcpy (host, &(node->rec), sizeof (struct Rec_host));

  return 0;
}

int
db_set_client_status (int id, enum STATUS new_status)
{
  extern struct Node_host* hosts_linked_list;
  struct Node_host* node = get_host_by_id (id, hosts_linked_list);
  
  if (node == NULL)
    {
      return -1;
    }
  
  node->rec.status = new_status;
  return 0;
}

/* Functions for working with proxies */

int
db_add_proxy (struct Rec_proxy* proxy)
{
  extern struct Node_proxy* proxies_linked_list;
  extern int                   proxies_count;
  
  struct Node_proxy* new_node;
  
  if (proxies_linked_list == NULL)
    {
      proxies_linked_list =
	(struct Node_proxy*) malloc (sizeof (struct Node_proxy));
      
      proxies_linked_list->prev = NULL;
      new_node = proxies_linked_list;
    }
  else
    {
      new_node = get_free_node_for_proxy(proxies_linked_list);
      if (new_node == NULL)
	{
	  syslog (LOG_WARNING, "%s: Unable to allocate memory "
		  "for new node in DB", DB_NAME);

	 return -1;
	}
    }
  
  ++proxies_count;

  strcpy(new_node->rec.name, proxy->name);
  new_node->rec.fd = proxy->fd;
  
  new_node->next = NULL;
 
  return 0;
}

int
db_rem_proxy (const int fd)
{
  extern struct Node_proxy* proxies_linked_list;
  extern int                proxies_count;
  
  struct Node_proxy* node = get_proxy_by_fd (fd, proxies_linked_list);
  
  if (node == NULL)
    {
      syslog (LOG_ERR, "%s: Proxy with fd=%d isn't found.", DB_NAME, fd);
      return -1;
    }
  
  syslog (LOG_DEBUG, "%s: proxy with fd = %d, name = %s",
	  DB_NAME, fd, node->rec.name);

  if (node->prev)
    node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;

  free (node);

  --proxies_count;
  
  return 0;
}

int
db_get_proxy_by_fd (const int fd, struct Rec_proxy* rec_proxy)
{
  extern struct Node_proxy* proxies_linked_list;
  
  struct Node_proxy* node = get_proxy_by_fd (fd, proxies_linked_list);
  
  if (node == NULL)
    {
      syslog (LOG_WARNING, "%s: There is no proxy with given FD = %d",
	      DB_NAME, fd);
      return -1;
    }
  memcpy (rec_proxy, &node->rec, sizeof (struct Rec_proxy));
  return 0;
}

int
db_get_proxy_by_name (const char* name, struct Rec_proxy* proxy)
{
  extern struct Node_proxy* proxies_linked_list;

  struct Node_proxy* node = get_proxy_by_name (name, proxies_linked_list);

  if (node == NULL)
    {
      syslog (LOG_WARNING, "%s: There is no proxy with given name = %s",
	      DB_NAME, name);
      return -1;
    }
  
  memcpy (proxy, &node->rec, sizeof (struct Rec_proxy));
  return 0;
}

int
db_get_proxies_list (int* count, int* list[])
{
  struct Node_proxy* node;
  int idx;

  if ((count == NULL) || (list == NULL))
    {
      return -1;
    }

  *list = (int*) calloc (proxies_count, sizeof (int));
  if (*list == NULL)
    {
      syslog (LOG_WARNING, "%s: Unable to allocate memory.", DB_NAME);
      return -1;
    }

  *count = proxies_count;

  for (node = proxies_linked_list, idx = 0;
       node && (idx < *count);
       node = node->next, ++idx)
    {
      *list[idx] = node->rec.fd;
    }
  
  return 0;
}

/*
 *   Static functions
 * ==============================================================================
 */

static struct Node_host*
get_free_node_for_host (struct Node_host* head)
{
  if (head->next == NULL)
    {
      head->next = (struct Node_host*) malloc (sizeof (struct Node_host));
      head->next->prev = head;
      head->next->next = NULL;
      return head->next;
    }

  return get_free_node_for_host (head->next);
}

static struct Node_host*
get_host_by_id (const int id, struct Node_host* head)
{
  if ((head->rec.id == id) || (head->next == NULL))
    return head;
  return get_host_by_id (id, head->next);
}

static struct Node_proxy*
get_free_node_for_proxy(struct Node_proxy* head)
{
  if (head->next == NULL)
    {
      head->next = (struct Node_proxy*) malloc (sizeof (struct Node_proxy));
      head->next->prev = head;
      head->next->next = NULL;      
      return head->next;
    }

  return get_free_node_for_proxy (head->next);
}

static struct Node_proxy*
get_proxy_by_fd (const int fd, struct Node_proxy* head)
{
  if ((head->rec.fd == fd) || (head->next == NULL))
    return head;
  
  return get_proxy_by_fd (fd, head->next);
}

static struct Node_proxy*
get_proxy_by_name (const char* name, struct Node_proxy* head)
{
  if ((strcmp(head->rec.name, name) == 0) || (head->next == NULL))
    return head;
  
  return get_proxy_by_name(name, head->next);
}
