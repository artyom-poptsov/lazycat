#ifndef __MACRO_H__
#define __MACRO_H__

/*
 * Nice macros for printing traces
 */

#define SYSLOG_ERROR(...)   syslog (LOG_ERR,     "-*- " __VA_ARGS__)
#define SYSLOG_INFO(...)    syslog (LOG_INFO,    "-i- " __VA_ARGS__)
#define SYSLOG_WARNING(...) syslog (LOG_WARNING, "-!- " __VA_ARGS__)
#define SYSLOG_DEBUG(...)   syslog (LOG_DEBUG,   "--- " __VA_ARGS__)
#define SYSLOG_RECV(...)    syslog (LOG_DEBUG,   "<-- " __VA_ARGS__)
#define SYSLOG_SEND(...)    syslog (LOG_DEBUG,   "--> " __VA_ARGS__)

enum { FALSE, TRUE };

#endif
