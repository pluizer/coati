#ifndef __audio_h__
#define __audio_h__

typedef struct _CT_Sample
{
	
} CT_Sample;

typedef struct _CT_Music
{
} CT_Music;

extern CT_Sample* load_sample(const char* filename);

extern void play_sample(CT_Sample* sample);

#endif /* __audio_h__ */
