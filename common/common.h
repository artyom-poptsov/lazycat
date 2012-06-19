#ifndef __MACRO_H__
#define __MACRO_H__

#define HANDLE_ERROR(...)					\
  do {								\
    syslog(LOG_ERR, __VA_ARGS__);				\
    exit(EXIT_FAILURE);						\
  } while (FALSE)

enum { FALSE, TRUE };

#endif
