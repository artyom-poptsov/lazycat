/* API which is used in the Scheme code.
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
#include <syslog.h>

#include "common.h"
#include "scmapi.h"
#include "db.h"

/*
 * Static functions
 */

static void lazycat_throw (char *message);

static int send_msg_to_host (const int host_id,
			     char* msg,
			     char* response[]);

static int update_host (const int host_id, char *field, char *value);

/*
 * This function sends message to the remote host and returns response.
 */
SCM
scm_send_msg (SCM dest, SCM msg)
{
  int host_id;
  char* msg_buf;
  char* response;
  int retval;

  SCM_ASSERT (SCM_NUMBERP (dest), msg, SCM_ARG1, __func__);
  SCM_ASSERT (scm_is_string (msg),  msg, SCM_ARG1, __func__);

  host_id = scm_to_int (dest);
  msg_buf = scm_to_locale_string (msg);

  retval = send_msg_to_host (host_id, msg_buf, &response);
  if (retval < 0)
    {
      SYSLOG_WARNING ("%s (host ID=%d)", response, host_id);
      lazycat_throw (response);
    }

  return scm_from_locale_string (response);
}

static int
send_msg_to_host (const int host_id, char* msg, char* response[])
{
  static const char ERROR_NO_CLIENT[]   = "Client with given ID is not found.";
  static const char ERROR_NO_PROXY[]    = "Proxy not found.";
  static const char ERROR_SEND_FAILED[] = "Message sending failed.";
  static const char ERROR_RECV_FAILED[] = "Message receiving failed.";

  struct db_rec proxy;
  struct db_rec host;

  char* msg_buf;
  int   msg_size;
  int32_t retval;

  int32_t *proxy_list;
  size_t  list_size;

  retval = db_get (host_id, &host);
  if (retval < 0)
    {
      *response = strdup (ERROR_NO_CLIENT);
      return -1;
    }

  SYSLOG_DEBUG ("Send a message");

  retval = db_query (DB_COL_NAME, host.proxy_name,
		     &list_size, &proxy_list);
  if (retval < 0)
    {
      *response = strdup (ERROR_NO_PROXY);
      return -1;
    }

  retval = db_get (proxy_list[0], &proxy);
  if (retval < 0)
    {
      *response = strdup (ERROR_NO_PROXY);
      return -1;
    }

  /*
   * Send host address
   */

  msg_buf = strdup (host.addr);
  msg_size = strlen (msg_buf);

  retval = xsend_msg (proxy.fd, msg_buf, msg_size);
  if (retval < 0)
    {
      *response = strdup (ERROR_SEND_FAILED);
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
      *response = strdup (ERROR_SEND_FAILED);
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
      *response = strdup (ERROR_RECV_FAILED);
      return -1;
    }

  SYSLOG_RECV ("Response: %s", *response);

  return 0;
}

/*
 * This function is used for adding new hosts to DB
 */
SCM
scm_add_host (SCM proxy_name, SCM address, SCM name, SCM description)
{
  struct db_rec rec;
  
  char* buf;
  int32_t retval;
  
  SCM_ASSERT (scm_is_string (proxy_name),  proxy_name,  SCM_ARG1, __func__);
  SCM_ASSERT (scm_is_string (address),     address,     SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_string (name),        name,        SCM_ARG3, __func__);
  SCM_ASSERT (scm_is_string (description), description, SCM_ARG4, __func__);

  rec.type = DB_REC_HOST;

  rec.proxy_name = scm_to_locale_string (proxy_name);
  rec.addr       = scm_to_locale_string (address);
  rec.name       = scm_to_locale_string (name);
  rec.desc       = scm_to_locale_string (description);

  retval = db_insert (&rec);

  free (rec.proxy_name);
  free (rec.addr);
  free (rec.name);
  free (rec.desc);

  return scm_from_int (retval);
}

/*
 * This function removes host with given host_id from DB
 */
SCM
scm_rem_host (SCM host_id)
{
  int id;
  int retval;
  
  SCM_ASSERT (SCM_NUMBERP (host_id), host_id, SCM_ARG1, __func__);

  id = scm_to_int (host_id);

  retval = db_delete (id);

  return (retval == 0) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
scm_update_host (SCM host_id, SCM field, SCM value)
{
  int32_t res;

  int32_t c_host_id;
  char    *c_field;
  char    *c_value;

  SCM_ASSERT (SCM_NUMBERP (host_id), host_id, SCM_ARG1, __func__);
  SCM_ASSERT (scm_is_string (field), field,   SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_string (value), value,   SCM_ARG3, __func__);

  c_host_id = scm_to_int (host_id);
  c_field   = scm_to_locale_string (field);
  c_value   = scm_to_locale_string (value);

  res = update_host (c_host_id, c_field, c_value);

  free (c_field);
  free (c_value);

  return (res == 0) ? SCM_BOOL_T : SCM_BOOL_F;
}

static int32_t
update_host (const int32_t host_id, char *field, char *value)
{
  struct db_rec rec;
  int32_t res = 0;

  res = db_get (host_id, &rec);
  if (res != 0)
    goto end;

  if (! strcmp (field, "proxy_name"))
    {
      free (rec.proxy_name);
      rec.proxy_name = strdup (value);
    }
  else if (! strcmp (field, "address"))
    {
      free (rec.addr);
      rec.addr = strdup (value);
    }
  else if (! strcmp (field, "name"))
    {
      free (rec.name);
      rec.name = strdup (value);
    }
  else if (! strcmp (field, "description"))
    {
      free (rec.desc);
      rec.desc = strdup (value);
    }
  else
    {
      res = -1;
      goto end;
    }
  
  res = db_update (host_id, &rec);

 end:
  free (rec.proxy_name);
  free (rec.addr);
  free (rec.name);
  free (rec.desc);

  return res;
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

  int32_t type = DB_REC_HOST;

  /* Get list of hosts from DB */
  
  retval = db_query (DB_COL_TYPE, &type, &count, &list);
  if (retval < 0)
    return SCM_BOOL_F;

  /* Convert C-array into Scheme-vector */
  
  vector = scm_make_vector (scm_from_int (count), scm_from_int (0));
  elt = scm_vector_writable_elements (vector, &handle, &len, &inc);

  for (elem_idx = 0; elem_idx < len; elt += inc, ++elem_idx)
    *elt = scm_from_uint (list[elem_idx]);

  scm_array_handle_release (&handle);

  return vector;
}

static void 
lazycat_throw (char *message)
{
  scm_throw (scm_from_locale_symbol ("lazycat-builtins-error"),
	     scm_from_locale_string (message));
}
