#ifndef __MY_LOG_H__
#define __MY_LOG_H__

#ifndef CONFIG_LOG_LEVEL
#define CONFIG_LOG_LEVEL 4
#endif


enum class LOG_LEVELS
{
    DEBUG = 1,
    INFO = 2,
    WARN = 3,
    ERROR = 4,
    NO_LOG = 5
};

#include <cstdio>

// TODO
#define LOG(LEVEL, format, ...)                                                           \
	do {                                                                                  \
		if ((LOG_LEVELS::LEVEL) >= (LOG_LEVELS) CONFIG_LOG_LEVEL)                         \
	    {                                                                                 \
			fprintf(stdout, "%s:%d:%s: ", __FILE__, __LINE__, __func__);                  \
			fprintf(stdout, format,## __VA_ARGS__);                                       \
			fprintf(stdout, "\n");                                                        \
		}                                                                                 \
	} while(0)

#endif // __MY_LOG_H__
