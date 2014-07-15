#include "audio.h"
#include <SDL2/SDL_mixer.h>
#include <math.h>

/* No need to bring in the whole of core.h */
extern const char* ct_get_error();
extern void ct_set_error(const char* str);
extern void* smalloc(size_t size);
#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b)) 
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

static int is_sound_inited = 0;

static int init_sound()
{
	if (!is_sound_inited)
	{
		int flags = MIX_INIT_OGG;
		int is_inited = Mix_Init(flags);
		if (is_inited != flags)
		{
			char str[1024];
			sprintf(str, "Could init mixer: %s", Mix_GetError());
			ct_set_error(str);
			return 1;
		}
		if(Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) == -1)
		{
			printf("!");
			char str[1024];
			sprintf(str, "Could init mixer: %s", Mix_GetError());
			ct_set_error(str);
			return 1;
		}
	}
	is_sound_inited = 1;
	return 0;
}

CT_Sample* ct_sample_load(const char* filename)
{
	if (init_sound()) return NULL;
	Mix_Chunk* chunk = Mix_LoadWAV(filename);
	if (!chunk)
	{
		char str[1024];
		sprintf(str, "Could not load audio file: %s, %s", 
			filename, Mix_GetError());
		ct_set_error(str);
	}
	CT_Sample* sample = smalloc(sizeof(CT_Sample));
	sample->mix_chunk = chunk;
	return sample;
}

void ct_sample_free(CT_Sample* sample)
{
	Mix_FreeChunk(sample->mix_chunk);
	free(sample);
}

/* Sample radius */

static float sample_radius = 1.0;

void ct_sample_radius_set(float value)
{
	sample_radius = value;
}

float ct_sample_radius()
{
	return sample_radius;
}

static void calc_mix_angle_and_distance(float* pos, Sint16* r_angle, Uint8* r_distance)
{
	/* Assumes listener is at the centre of the screen */
	float delta_x = pos[0] - .5;
	float delta_y = pos[1] - .5;
	float dist = sqrt(delta_x*delta_x + delta_y*delta_y);
	*r_distance = (Uint8)MIN((255.0 / sample_radius) * dist, 255);
	/* Fixme: doesnt really work */
	/* *r_angle = atan2(delta_y, delta_x) * (180 / 3.14); */
	*r_angle = (delta_x > 0) ? 90 : 270;
	printf("%f, %d\n", delta_x, *r_angle);
}

/* Channel */

CT_Channel ct_sample_play(CT_Sample* sample, float* position, int loop)
{
	Sint16 angle;
	Uint8  dist;
	calc_mix_angle_and_distance(position, &angle, &dist);
	CT_Channel channel = Mix_PlayChannel(-1, sample->mix_chunk, loop);
	Mix_SetPosition(channel, angle, dist);
	return channel;
}
