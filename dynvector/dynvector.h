#ifndef __dynvector_h_
#define __dynvector_h_

typedef struct
{
	unsigned* data;
	unsigned max_size;
	unsigned size;
} IndexStack;

typedef struct DynVector_t
{
	float* data;
	unsigned* indices;
	IndexStack* available_stack;
	IndexStack* last_stack;
	unsigned size;
	unsigned max_size;
	unsigned chunk_size;
} DynVector;

extern DynVector* new_dynvector(unsigned chunk_size, unsigned max_size);

extern void free_dynvector(DynVector* dv);

extern unsigned dynvector_push(DynVector* dv, float* chunk);

extern void dynvector_remove(DynVector* dv, unsigned index);

extern void dynvector_change(DynVector* dv, unsigned index, float* chunk);

extern float* dynvector_get(DynVector* dv, unsigned index);

#endif /* __dynvector_h_ */
