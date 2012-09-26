#ifndef __PARSERS_H__
#define __PARSERS_H__

int32_t get_username_from_address (const char *str, char **user_name);
int32_t get_hostname_from_address (const char *str, char **host_name);
int32_t get_port_from_address (const char *str, uint16_t *port);

#endif /* ifndef __PARSERS_H__ */
