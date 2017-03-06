#pragma once

#include <pthread.h>

extern "C" {
	int pthread_mutex_lock(pthread_mutex_t *mutex) noexcept;
}