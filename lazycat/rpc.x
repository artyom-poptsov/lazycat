/* -*- mode: c; -*- */

/* rpc.x -- Protocol definition for lazycat.
 *
 * Definition of the protocol which is used for communication between
 * a lazycat daemon and a UI.  The protocol is written in XDR
 * language (see RFC 1832).
 *
 * Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of lazycat.
 *
 * lazycat is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * lazycat is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with lazycat.  If not, see <http://www.gnu.org/licenses/>.
 */

const HOST_NAME_LEN  = 256;
const GROUP_NAME_LEN = 256;
const PROXY_NAME_LEN = 256;
const ADDR_LEN       = 256;

enum result_t {
  SUCCESS =  0,
  ERROR   = -1
};

enum diff_action_t {
  GET_PATTERN = 0,
  CONTINUE    = 1,
  ABORT       = 2
};


/* A list of hosts */
typedef struct host *hosts;
struct host {
  string name<HOST_NAME_LEN>;
  string group<GROUP_NAME_LEN>;
  string proxy<PROXY_NAME_LEN>;
  string address<ADDR_LEN>;
  string description<>;
};


/* Exec arguments */
struct exec_args {
  string command<>;             /* Command to execute */

  /* ID of the host.  -1 means that the command should be executed on
     all hosts. */
  int    host_id;

  bool   translate;             /* `true' if command should be translated */
};


program LAZYCAT_PROGRAM
{
  version LAZYCAT_VERSION
  {
    result_t
      lc_add_host (host) = 1;

    result_t
      lc_rem_host (int) = 2;

    result_t                    /* TODO: Fix it. */
      lc_list (string) = 3;

    result_t
      lc_exec (exec_args) = 4;

    /* result_t */
    /*   lc_cmd_diff (diff_action_t, */
    /*                string) = 5;      /\* command *\/ */

    /* result_t */
    /*   lc_get (string) = 6; */

    /* result_t */
    /*   lc_set (string, string) = 7; */

    result_t
      lc_stop (void) = 8;
                   
  } = 0;
} = 25556;
