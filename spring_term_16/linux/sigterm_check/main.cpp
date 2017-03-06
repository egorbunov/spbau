#include <iostream>
#include <cstdio>
#include <signal.h>
#include <setjmp.h>
#include <cassert>

static sigjmp_buf check_pointer_ctx;

void handler(int signum)
{
	std::cout << "SEGFAULT!" << std::endl;
	siglongjmp(check_pointer_ctx, 1);
	return;
}

bool check_pointer(void* ptr) {
	struct sigaction action;
	action.sa_handler = handler;
	sigaction(SIGSEGV, &action, NULL);

	if (sigsetjmp(check_pointer_ctx, 1) == 0) {
		char x = *((char*) ptr);
	} else {
		return false;
	}
	return true;
}

int main (void)
{
	assert(check_pointer(0) == false);

	return 0;
}