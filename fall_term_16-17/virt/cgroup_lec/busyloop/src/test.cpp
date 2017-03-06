#include <cstdio>
#include <cstdlib>

#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>

const int DURATION_SEC = 10; // provides stable results
volatile bool all_stop = false;
size_t global_result = 0;

void handle_timeout(int signal)
{
    (void)signal;
    all_stop = true;
}

void setup_timeout()
{
    struct sigaction sa;
    sa.sa_handler = handle_timeout;
    sa.sa_flags = SA_RESTART;
    sigfillset(&sa.sa_mask);
    sigaction(SIGALRM, &sa, NULL);
    alarm(DURATION_SEC);
}

void* thread_func(void* arg)
{
    (void)arg;
    size_t my_result = 0;
    while(!all_stop) {
        ++my_result;
    }
    __sync_add_and_fetch(&global_result, my_result);
    return NULL;
}

// Spawns one thread for each logical CPU.
// Each thread does busyloops for DURATION_SEC seconds
// Numbers of loops is summed and outputed to stdout
int main(int argc, char **argv)
{
    int num_threads = sysconf(_SC_NPROCESSORS_ONLN);
    //printf("Spawning %d threads\n", num_threads);

    pthread_t *threads = new pthread_t[num_threads];
    pthread_attr_t thread_attrs;
    pthread_attr_init(&thread_attrs);

    for (int i = 0; i < num_threads; ++i)
    {
        int err = pthread_create(&threads[i], &thread_attrs,
                thread_func, NULL);
        if (err != 0)
        {
            errno = err;
            perror("Thread creation failed");
            return 1;
        }
    }

    pthread_attr_destroy(&thread_attrs);
    setup_timeout();

    for (int i = 0; i < num_threads; ++i)
    {
        pthread_join(threads[i], NULL);
    }

    printf("%zu\n", global_result);

    delete[] threads;
    return 0;
}
