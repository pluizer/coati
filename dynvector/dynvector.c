#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "dynvector.h"

IndexStack* new_index_stack(unsigned max_size)
{
	IndexStack* is = malloc(sizeof(IndexStack));
	is->data = malloc(sizeof(unsigned) * max_size);
	is->max_size = max_size;
	is->size = 0;
	return is;
}

void free_index_stack(IndexStack* is)
{
	free(is->data);
	free(is);
}

void index_stack_push(IndexStack* is, unsigned index)
{
	assert(is->size < is->max_size);
	is->data[is->size++] = index;
}

int index_stack_empty(IndexStack* is)
{
	return !is->size;
}

unsigned index_stack_pop(IndexStack* is)
{
	assert(!index_stack_empty(is));
	return is->data[is->size - 1];
}

DynVector* new_dynvector(unsigned chunk_size, unsigned max_size)
{
	DynVector* dv = malloc(sizeof(DynVector));
	assert(dv);
	dv->indices = malloc(sizeof(unsigned)*max_size);
	memset(dv->indices, 0, sizeof(unsigned)*max_size);
	dv->available_stack = new_index_stack(max_size);
	dv->last_stack = new_index_stack(max_size);
	dv->size = 0;
	dv->max_size = max_size;
	dv->chunk_size = chunk_size;
	dv->data = malloc(dv->chunk_size * sizeof(float) * max_size);
	memset(dv->data, 0, dv->chunk_size * sizeof(float) * max_size);
	return dv;
}

void free_dynvector(DynVector* dv)
{
	free(dv->data);
	free(dv->indices);
	free_index_stack(dv->available_stack);
	free_index_stack(dv->last_stack);
	free(dv);
}


unsigned dynvector_push(DynVector* dv, float* chunk)
{
	unsigned index = index_stack_empty(dv->available_stack) 
		? dv->size : index_stack_pop(dv->available_stack);
	assert(dv->size < dv->max_size);
	memcpy(dv->data+(dv->size*dv->chunk_size), chunk, dv->chunk_size*sizeof(float));
	dv->indices[dv->size] = index;
	dv->size++;
	index_stack_push(dv->last_stack, index);
	return index;
}

void dynvector_remove(DynVector* dv, unsigned index)
{
	dv->size--;
	if (dv->indices[index] != dv->size)
	{
		memcpy(dv->data+(dv->chunk_size * dv->indices[index]),
		       dv->data+(dv->chunk_size * (dv->size)),
		       dv->chunk_size * sizeof(float));
		dv->indices[index_stack_pop(dv->last_stack)] = dv->indices[index];
	}
	index_stack_push(dv->available_stack, index);
}

void dynvector_change(DynVector* dv, unsigned index, float* chunk)
{
	memcpy(dv->data+(dv->chunk_size * dv->indices[index]),
	       chunk,
	       dv->chunk_size * sizeof(float));
}

float* dynvector_get(DynVector* dv, unsigned index)
{
	return dv->data+(dv->chunk_size * dv->indices[index]);
}
