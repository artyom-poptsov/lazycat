/* Copyright (C) 2013 Artyom Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __LIBSSH_COMPAT_H__
#define __LIBSSH_COMPAT_H__

/* There are changes in libssh API between version 0.4.x and 0.5.x.
   The most significant changes for us are that some API functions
   have changed their names.  So if we try to build LazyCat against
   libssh 0.4.x, we will get 'undefined reference' errors.

   To solve this issue, I just replace new function names with the new
   ones.  Probably that's not a great solution, so if you find some
   bad side effects/bugs, don't hesitate to write me about it at
   <poptsov.artyom@gmail.com>.

   - avp */

#ifdef LIBSSH_0_4

/* OK, here we dealing with the old libssh API. */

#define ssh_channel_new channel_new
#define ssh_channel_open_session channel_open_session
#define ssh_channel_request_exec channel_request_exec
#define ssh_channel_poll channel_poll
#define ssh_channel_free channel_free
#define ssh_channel_read channel_read

#endif  /* ifdef LIBSSH_0_4 */


#endif /* ifndef __LIBSSH_COMPAT_H__ */

/* libssh-compat.h ends here. */
