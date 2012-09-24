/* API which is used in the Scheme code.
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

#include <libguile.h>
#include <syslog.h>

#include "common.h"
#include "scmapi.h"
#include "db.h"

/*
 * External variables and functions
 */

extern int db_add_host (struct Rec_host* host);
extern int db_rem_host (int id);
extern int db_get_host_record (const int id, struct Rec_host* host);
extern int db_get_proxy_by_name (const char* name, struct Rec_proxy* proxy);
extern int db_get_hosts_list (int* count, int* list[]);

/*
 * Static functions
 */

static int send_msg_to_host (const int host_id,
			     char* msg,
			     char* response[]);

static char* calcpy (char** dest, const char* src);

/*
 * This function sends message to the remote host and returns response.
 */
SCM
scm_send_msg (SCM dest, SCM msg)
{
  const static char SUBR_NAME[] = "scm_send_msg";
  
  int host_id;
  char* msg_buf;
  char* response;
  int retval;

  SCM_ASSERT (SCM_NUMBERP (dest), msg, SCM_ARG1, SUBR_NAME);
  SCM_ASSERT (SCM_STRINGP (msg),  msg, SCM_ARG1, SUBR_NAME);

  host_id = scm_to_int (dest);
  msg_buf = scm_to_locale_string (msg);

  retval = send_msg_to_host (host_id, msg_buf, &response);
  if (retval < 0)
    SYSLOG_WARNING ("%s (host ID=%d)", response, host_id);

  return scm_from_locale_string (response);
}

static int
send_msg_to_host (const int host_id, char* msg, char* response[])
{
  static const char ERROR_NO_CLIENT[]   = "Client with given ID is not found.";
  static const char ERROR_NO_PROXY[]    = "Proxy not found.";
  static const char ERROR_SEND_FAILED[] = "Message sending failed.";
  static const char ERROR_RECV_FAILED[] = "Message receiving failed.";

  struct Rec_proxy proxy;
  struct Rec_host  host;

  char* msg_buf;
  int   msg_size;
  int   retval;

  retval = db_get_host_record (host_id, &host);
  if (retval < 0)
    {
      calcpy (response, ERROR_NO_CLIENT);
      return -1;
    }

  SYSLOG_DEBUG ("Send a message");

  retval = db_get_proxy_by_name (host.proxy_name, &proxy);
  if (retval < 0)
    {
      calcpy (response, ERROR_NO_PROXY);
      return -1;
    }
  
  /*
   * Send host address
   */

  calcpy (&msg_buf, host.address);
  msg_size = strlen (msg_buf);
  
  retval = xsend_msg (proxy.fd, msg_buf, msg_size);
  if (retval < 0)
    {
      calcpy (response, ERROR_SEND_FAILED);
      return -1;
    }

  SYSLOG_SEND ("Destination ID: %s", msg_buf);

  free (msg_buf);

  /*
   * Send message
   */

  msg_size = strlen (msg);

  retval = xsend_msg (proxy.fd, msg, msg_size);
  if (retval < 0)
    {
      calcpy (response, ERROR_SEND_FAILED);
      return -1;
    }

  SYSLOG_SEND ("Messsage: %s", msg);

  /*
   * Receive response
   */

  SYSLOG_DEBUG ("Receive a response");

  retval = xrecv_msg (proxy.fd, response, &msg_size);
  if (retval < 0)
    {
      calcpy (response, ERROR_RECV_FAILED);
      return -1;
    }

  SYSLOG_RECV ("Response: %s", *response);

  return 0;
}

/*
 * This function allocates memory and makes a copy of string src
 */
static char*
calcpy (char** dest, const char* src)
{
  size_t str_len = strlen (src);
  *dest = (char*) calloc (str_len, sizeof (char));
  return strncpy (*dest, src, str_len);
}

/*
 * This function is used for adding new hosts to DB
 */
SCM
scm_add_host (SCM proxy_name, SCM address, SCM name, SCM description)
{
  const static char SUBR_NAME[] = "scm_add_host";
  struct Rec_host host;
  
  char* buf;
  int   retval;
  
  SCM_ASSERT (SCM_STRINGP (proxy_name),  proxy_name,  SCM_ARG1, SUBR_NAME);
  SCM_ASSERT (SCM_STRINGP (address),     address,     SCM_ARG2, SUBR_NAME);
  SCM_ASSERT (SCM_STRINGP (name),        name,        SCM_ARG3, SUBR_NAME);
  SCM_ASSERT (SCM_STRINGP (description), description, SCM_ARG4, SUBR_NAME);

  buf = scm_to_locale_string (proxy_name);
  strncpy (host.proxy_name, buf, MAX_PROXY_NAME_LEN);

  free (buf);

  buf = scm_to_locale_string (address);
  strncpy (host.address, buf, MAX_ADDRESS_LEN);

  free (buf);

  buf = scm_to_locale_string (name);
  strncpy (host.name, buf, MAX_HOST_NAME_LEN);

  free (buf);

  buf = scm_to_locale_string (description);
  strncpy (host.name, buf, MAX_HOST_DESC_LEN);

  free (buf);

  retval = db_add_host (&host);

  return scm_from_int (retval);
}

/*
 * This function removes host with given host_id from DB
 */
SCM
scm_rem_host (SCM host_id)
{
  const static char SUBR_NAME[] = "scm_rem_host";
  int id;
  int retval;
  
  SCM_ASSERT (SCM_NUMBERP (host_id), host_id, SCM_ARG1, SUBR_NAME);

  id = scm_to_int (host_id);

  retval = db_rem_host (id);
  if (retval < 0)
    {
      return SCM_BOOL_F;
    }
  else
    {
      return SCM_BOOL_T;
    }
}

SCM
scm_update_host (SCM host_id, SCM field, SCM value)
{
  const static char SUBR_NAME[] = "scm_update_host";
  int retval;
  
  SCM_ASSERT (SCM_NUMBERP (host_id), host_id, SCM_ARG1, SUBR_NAME);
  SCM_ASSERT (scm_is_string (field), field,   SCM_ARG2, SUBR_NAME);
  SCM_ASSERT (scm_is_string (value), value,   SCM_ARG3, SUBR_NAME);

  retval = db_update_host (scm_to_int (host_id),
			   scm_to_locale_string (field),
			   scm_to_locale_string (value));

  if (retval < 0)
    {
      return SCM_BOOL_F;
    }
  else
    {
      return SCM_BOOL_T;
    }
}

/*
 * This function returns list of hosts
 */
SCM
scm_get_host_list (void)
{
  int       count;
  int*      list;
  
  size_t  elem_idx; /* Element index */
  size_t  len;      /* Vector lenght */
  ssize_t inc;      /* Increment size */

  int retval;

  scm_t_array_handle handle;
  SCM  vector;
  SCM* elt;

  /* Get list of hosts from DB */
  
  retval = db_get_hosts_list (&count, &list);
  if (retval < 0)
    return SCM_BOOL_F;

  /* Convert C-array into Scheme-vector */
  
  vector = scm_make_vector (scm_from_int (count), scm_from_int (0));
  elt = scm_vector_writable_elements (vector, &handle, &len, &inc);

  for (elem_idx = 0; elem_idx < len; elt += inc, ++elem_idx)
    {
      *elt = scm_from_uint (list[elem_idx]);
    }

  scm_array_handle_release (&handle);

  return vector;
}
