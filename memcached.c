#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/eventfd.h>
#include <signal.h>
#include "commons/sock.h"
#include "server.h"

int mock_event;

void limit_mem(size_t limit)
{
	struct rlimit mem_limit;

    if (getrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("getrlimit");
        exit(EXIT_FAILURE);
    }

    // mem_limit.rlim_cur = 1073741824;  // 1 GB
	mem_limit.rlim_max = limit;
	mem_limit.rlim_cur = limit;
	printf("%zu\n", limit);

    if (setrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
    }
}

void handle_signals(int s) {
	switch (s)
	{
	case SIGPIPE:{
		log(1, "SIGPIPE ERROR");
		break;
	}
	case SIGTSTP:{
		log(1, "SIGSTP");
		terminate_threads=1;
		uint64_t dummy=50;
		write(mock_event, &dummy, sizeof(uint64_t));
		break;
	}
	case SIGINT:{
		log(1, "SIGINT");
		terminate_threads=1;
		uint64_t dummy=50;
		write(mock_event, &dummy, sizeof(uint64_t));
		break;
	}
	default:
		break;
	}
}

int main(int argc, char **argv)
{
	int text_sock, bin_sock;

	__loglevel = 2;
	//Magic number: 11400000 1 thread, 5 keys
	//Magic number: 36470000 4 threads, 1 key
	limit_mem(1073741824);
	signal(SIGPIPE, handle_signals);
	signal(SIGTSTP, handle_signals);
	signal(SIGINT, handle_signals);
	hashtable_create(1);
	queue_create();

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	mock_event = eventfd(0,0);
	if (mock_event < 0)
		quit("eventfd");

	server(text_sock, bin_sock, mock_event);
	queue_destroy();
	hashtable_destroy();
	return 0;
}
