/* Homebrew DB for storing information about hosts.
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
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>

#include "common.h"
#include "db.h"


/*
 * Types
 */

struct db_node
{
  /* Unique ID of the node. */
  int32_t id;

  struct db_rec rec;

  struct db_node* prev;
  struct db_node* next;
};

struct db_selection_elem
{
  struct db_node *node;
  struct db_selection_elem *next;
};

typedef bool (*predicate_t)(struct db_node *, void *);


/*
 * Global definitions
 */

static const char DB_NAME[] = "db";

static int32_t rec_count = 0;
static struct db_node* linked_list_head;


/*
 * Prototypes
 */

static struct db_node*
get_node_by_id (const int32_t id, struct db_node *head);

static struct db_node*
get_free_node (struct db_node* head);

static int32_t
get_list_by_type (const int32_t type, size_t *size, int32_t **list);

static int32_t
select_from (predicate_t pred, void *value,
	     struct db_node *head,
	     size_t *list_size,
	     struct db_selection_elem **list_head);

static bool is_type_equal (struct db_node *node, void *value);
static bool is_proxy_name_equal (struct db_node *node, void *value);
static bool is_name_equal (struct db_node *node, void *value);
static bool is_addr_equal (struct db_node *node, void *value);
static bool is_desc_equal (struct db_node *node, void *value);
static bool is_stat_equal (struct db_node *node, void *value);

static int32_t free_selection (struct db_selection_elem **head);


/*
 * Interface to the DB
 */

/* Add a new record REC to the DB. */
int32_t
db_insert (struct db_rec *rec)
{
  struct db_node *new_node;

  if (linked_list_head == NULL)
    {
      linked_list_head =
	(struct db_node*) malloc (sizeof (struct db_node));
      linked_list_head->prev = NULL;
      linked_list_head->next = NULL;
      new_node = linked_list_head;
    }
  else
    {
      new_node = get_free_node (linked_list_head);
      if (new_node == NULL)
	{
	  SYSLOG_WARNING ("%s: Unable to allocate memory "
		  "for new node in DB", DB_NAME);
	  return -1;
	}
    }

  new_node->id = ++rec_count;
  new_node->rec.fd = rec->fd;

  if (rec->proxy_name != NULL)
    new_node->rec.proxy_name = strdup (rec->proxy_name);
  if (rec->name != NULL)
    new_node->rec.name = strdup (rec->name);
  if (rec->addr != NULL)
    new_node->rec.addr = strdup (rec->addr);
  if (rec->desc != NULL)
    new_node->rec.desc = strdup (rec->desc);

  new_node->rec.stat = rec->stat;
  new_node->next = NULL;

  return new_node->id;
}

/* Remove a record with the given ID from the DB */
int32_t
db_delete (const int32_t id)
{
  struct db_node *node = get_node_by_id (id, linked_list_head);
  if (node == NULL)
    {
      SYSLOG_WARNING ("%s: Node with id=%d isn't found.", DB_NAME, id);
      return -1;
    }

  if (node->prev)
    node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;

  free (node);

  if (node == linked_list_head)
    linked_list_head = NULL;

  --rec_count;

  return 0;
}

/* Replace current record with id ID with a new one. */
int32_t
db_update (const int32_t id, struct db_rec *rec)
{
  struct db_node *node = get_node_by_id (id, linked_list_head);

  if ((rec == NULL) || (node == NULL))
    return -1;

  node->rec.fd = rec->fd;

  free (node->rec.proxy_name);
  if (rec->proxy_name != NULL)
    node->rec.proxy_name = strdup (rec->proxy_name);

  free (node->rec.name);
  if (rec->name != NULL)
    node->rec.name = strdup (rec->name);

  free (node->rec.addr);
  if (rec->addr != NULL)
    node->rec.addr = strdup (rec->addr);

  free (node->rec.desc);
  if (rec->desc != NULL)
    node->rec.desc = strdup (rec->desc);

  node->rec.stat = rec->stat;

  return 0;
}

/* Check that a host with the id ID is exists in DB.  This function
   works faster than db_get () because it does no copying of host'
   data. 
   Return 0 if a host is exist in the DB, or -1 if it isn't. */
int32_t
db_check (const int32_t id)
{
  return (get_node_by_id (id, linked_list_head) == NULL)? -1 : 0;
}

/* Get a record with the given ID.  String fields of the record should
   be freed after usage.
   Return 0 on success, -1 on failure. */
int32_t
db_get (const int32_t id, struct db_rec *rec)
{
  struct db_node *node = get_node_by_id (id, linked_list_head);

  if (node == NULL)
    return -1;

  rec->type = node->rec.type;
  rec->fd   = node->rec.fd;

  if (node->rec.proxy_name != NULL)
    rec->proxy_name = strdup (node->rec.proxy_name);
  if (node->rec.name != NULL)
    rec->name = strdup (node->rec.name);
  if (node->rec.addr != NULL)
    rec->addr = strdup (node->rec.addr);
  if (node->rec.desc != NULL)
    rec->desc = strdup (node->rec.desc);

  rec->stat = node->rec.stat;

  return 0;
}

/* Query DB for information.  The function searches a value VALUE in
   column COL and stores IDs of matched records in a newly allocated
   LIST of size SIZE.  List should be freed after usage.
   Return 0 on success, -1 on failure. */
int32_t
db_query (enum DB_COL col, void *value, size_t *size, int32_t **list)
{
  struct db_selection_elem *elem;
  struct db_selection_elem *selection = NULL;
  size_t selection_size = 0;
  int32_t res;

  uint32_t idx;

  switch (col)
  {
  case DB_COL_ID:
    res = select_from (is_type_equal, DB_WILDCARD,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_TYPE:
    res = select_from (is_type_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_PROXY_NAME:
    res = select_from (is_proxy_name_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_NAME:
    res = select_from (is_name_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_ADDR:
    res = select_from (is_addr_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_DESC:
    res = select_from (is_desc_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;

  case DB_COL_STAT:
    res = select_from (is_stat_equal, value,
		       linked_list_head,
		       &selection_size, &selection);
    break;
    
  default:
    SYSLOG_WARNING ("Operation not supported yet (col = %d)", col);
    return -1;
  }

  if (res)
    {
      free_selection (&selection);
      return -1;
    }

  *list = (int32_t *) malloc (sizeof (int32_t) * selection_size);
  if (*list == NULL)
    {
      free_selection (&selection);
      return -1;
    }

  for (idx = 0, elem = selection; elem != NULL; ++idx, elem = elem->next)
    (*list)[idx] = elem->node->id;

  free_selection (&selection);

  return 0;
}


/*
 * Static functions
 */

/* Select from the DB by executing a predicate PRED on an every record
   in the DB.  Selection will be stored as a list of size LIST_SIZE
   pointed by LIST_HEAD.  The list should be freed after usage.
   Return 0 on success, -1 on failure. */
static int32_t
select_from (predicate_t pred, void *value,
	     struct db_node *head,
	     size_t *list_size,
	     struct db_selection_elem **list_head)
{
  if (head == NULL)
    return 0;

  if (pred (head, value))
    {
      *list_head = (struct db_selection_elem *)
	calloc (1, sizeof (struct db_selection_elem));

      if (*list_head == NULL)
	return -1;

      (*list_head)->node = head;
      ++(*list_size);

      return select_from (pred, value, head->next, 
			  list_size, &((*list_head)->next));
    }
  return select_from (pred, value, head->next, list_size, list_head);
}

/* Predicates */

static bool
is_type_equal (struct db_node *node, void *value)
{
  enum DB_REC_TYPE type = *((enum DB_REC_TYPE *) value);
  return (node->rec.type == type);
}

static bool
is_proxy_name_equal (struct db_node *node, void *value)
{
  char *proxy_name = (char *) value;
  return ((node->rec.proxy_name != NULL)
	  && (! strcmp (node->rec.proxy_name, proxy_name)));
}

static bool
is_name_equal (struct db_node *node, void *value)
{
  char *name = (char *) value;
  return ((node->rec.name != NULL) && (! strcmp (node->rec.name, name)));
}

static bool
is_addr_equal (struct db_node *node, void *value)
{
  char *addr = (char *) value;
  return ((node->rec.addr != NULL) && (! strcmp (node->rec.addr, addr)));
}

static bool
is_desc_equal (struct db_node *node, void *value)
{
  char *desc = (char *) value;
  return ((node->rec.desc != NULL) && (! strcmp (node->rec.desc, desc)));
}

static bool
is_stat_equal (struct db_node *node, void *value)
{
  enum STATUS stat = *((enum STATUS *) value);
  return (node->rec.stat == stat);
}


/* Allocate memory for a new node.
   Return newly allocated node on success, NULL on failure. */
static struct db_node*
get_free_node (struct db_node* head)
{
  if (head->next == NULL)
    {
      head->next = (struct db_node*) malloc (sizeof (struct db_node));
      head->next->prev = head;
      head->next->next = NULL;
      return head->next;
    }

  return get_free_node (head->next);
}

/* Get a node with given ID */
static struct db_node*
get_node_by_id (const int32_t id, struct db_node *head)
{
  if ((head == NULL) || (head->id == id))
    return head;
  return get_node_by_id (id, head->next);
}

/* Free a selection. */
static int32_t
free_selection (struct db_selection_elem **head)
{
  if (*head == NULL)
    return 0;
  free_selection (&(*head)->next);
  free (*head);
  return 0;
}

/* db.c ends here. */
