#include <dlfcn.h>
#include <stdio.h>
#include <thread>
#include <unordered_map>
#include "libmypthread.h"

// globals
typedef int (*mutex_lock_f_t)(pthread_mutex_t*);
static std::unordered_map<std::thread::id, int> thread_ids;
static volatile int max_id; 

static void __attribute__ ((constructor)) libmypthread_init(void) {
	fprintf(stdout, "INIT\n");
	fflush(stdout);
}

static void __attribute__ ((destructor)) libmypthread_deinit(void) {
	fprintf(stdout, "DEINIT\n");
	fflush(stdout);
}

int pthread_mutex_lock(pthread_mutex_t *mutex) noexcept {

	auto tid = std::this_thread::get_id();
	if (thread_ids.find(tid) == thread_ids.end()) {
		thread_ids[tid] = max_id++;
	}

    fprintf(stdout, "Thread [%d] locking mutex [%p]\n", thread_ids[tid], mutex);
    fflush(stdout);

	mutex_lock_f_t real_mutex_lock = (mutex_lock_f_t) dlsym(RTLD_NEXT, "pthread_mutex_lock");

	return real_mutex_lock(mutex);
}

