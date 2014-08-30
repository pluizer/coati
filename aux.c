#include <stdlib.h>
#include <stdio.h>

void* smalloc(size_t size)
{
	void* ptr = malloc(size);
	if (!ptr)
	{
		fprintf(stderr, "Out of memory!");
		exit(-1);
	}
	return ptr;
}

void* srealloc(void* old, size_t size)
{
	void* ptr = realloc(old, size);
	if (!ptr)
	{
		fprintf(stderr, "Out of memory!");
		exit(-1);
	}
	return ptr;
}
