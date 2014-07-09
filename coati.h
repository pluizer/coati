#ifndef __core_h__
#define __core_h__

#define CT_MAX_TARGET_STACK_SIZE  32
#define CT_MAX_BLEND_STACK_SIZE   32
#define CT_MAX_COLOUR_STACK_SIZE  32
#define CT_MAX_INPUT_STACK_SIZE   8

#define CT_SCREEN ct_screen_texture()

#include <stdio.h>

typedef struct _DV_Vector DV_Vector;
typedef enum _CT_Key CT_Key;
typedef struct SDL_Surface SDL_Surface;
typedef struct SDL_Window SDL_Window;
typedef struct SDL_RWops SDL_RWops;
typedef struct _TTF_Font TTF_Font;

typedef struct _CT_Image
{
	SDL_Surface* sdl_surface;
} CT_Image;

typedef struct _CT_Texture
{
	unsigned w, h;
	unsigned gl_texture_id;
	unsigned gl_buffer_id;
} CT_Texture;

typedef struct
{
	SDL_Window* sdl_window;
	const char* title;
	int fullscreen;
	int is_size_changed;
} CT_Window;

typedef struct _CT_Batch
{
	DV_Vector* vector;
	unsigned short* indices;
	CT_Texture* atlas;
} CT_Batch;

typedef struct _CT_Transformation
{
	float src_rect[4];
	float dst_rect[4];
	float origin[2];
	float rotation;
	float flip_h, flip_v; /* positive = true, negative = false */
} CT_Transformation;

typedef struct _CT_Font
{
	FILE* file;
	SDL_RWops* rw;

	struct CT_FontMapLink
	{
		unsigned size;
		TTF_Font* value;
		struct CT_FontMapLink* next;
	} font_map;

	struct CT_FontMapLink* first;
	
} CT_Font;

/* Error */

extern const char* ct_get_error();

extern void ct_set_error(const char* str);

/* Window */

extern int ct_window_init();

extern void ct_window_quit();

extern void ct_window_res_set(int x, int y);

extern void ct_window_fullscreen_set(int fullscreen);

extern void ct_window_update();

/* Image */

extern CT_Image* ct_image_load(const char* filename);

extern CT_Image* ct_image_create(unsigned w, unsigned h);

extern void ct_image_free(CT_Image* image);

extern void ct_image_size(CT_Image* image, float* vect);

/* Colour */

extern void ct_push_colour(float* colour);

extern void ct_pop_colour();

/* Blending */

typedef enum _CT_BlendMode
{
	CT_BLEND_MODE_NORMAL,
	CT_BLEND_MODE_ADD,
	CT_BLEND_MODE_TRANS,
	CT_BLEND_MODE_ONE_ONE
} CT_BlendMode;

extern void ct_push_blend_mode(CT_BlendMode mode);

extern void ct_pop_blend_mode();

/* Texture */

extern CT_Texture* ct_image_to_texture(CT_Image* image);

extern CT_Texture* ct_texture_create(unsigned w, unsigned h);

extern CT_Texture* ct_texture_copy(CT_Texture* texture);

extern CT_Texture* ct_texture_load(const char* filename);

extern void ct_texture_free(CT_Texture* tex);

extern int ct_is_texture_screen(CT_Texture* tex);

extern void ct_texture_size(CT_Texture* texture, float* vect);

extern void ct_texture_clear(CT_Texture* tex, float* colour);

extern void ct_texture_render(CT_Texture* tex, float* matrix, CT_Transformation* trans);

extern CT_Texture* ct_screen_texture();

/* Target */

extern void ct_push_target(CT_Texture* tex);

extern void ct_pop_target();

/* Batch */

extern CT_Batch* ct_batch_create(CT_Texture* atlas, unsigned size);

extern void ct_batch_free(CT_Batch* batch);

extern unsigned ct_batch_push(CT_Batch* batch, CT_Transformation* trans);

extern void ct_batch_remove(CT_Batch* batch, unsigned id);

extern void ct_batch_change(CT_Batch* batch, unsigned id, CT_Transformation* trans);

extern void ct_batch_render(CT_Batch* batch, float* matrix);

/* Font */

extern CT_Font* ct_font_load(const char* filename);

extern void ct_font_free(CT_Font* font);

extern CT_Texture* ct_string_to_texture(CT_Font* font,
					unsigned size,
					const char* string,
					float* colour);

/* Input */

extern int ct_is_quitting();

extern int ct_key_pressed(CT_Key key);

extern int ct_key_released(CT_Key key);

extern int ct_key_holded(CT_Key key);

extern void ct_mouse_position(float* ret);

extern void ct_poll_input();

extern const char* ct_key_name(CT_Key key);

typedef enum _CT_Key
{
	CT_KEY_BACKSPACE = 8,
	CT_KEY_TAB = 9,
	CT_KEY_CLEAR = 12,
	CT_KEY_RETURN = 13,
	CT_KEY_PAUSE = 19,
	CT_KEY_ESCAPE = 27,
	CT_KEY_SPACE = 32,
	CT_KEY_EXCLAIM = 33,
	CT_KEY_QUOTEDBL = 34,
	CT_KEY_HASH = 35,
	CT_KEY_DOLLAR = 36,
	CT_KEY_AMPERSAND = 38,
	CT_KEY_QUOTE = 39,
	CT_KEY_LEFTPAREN = 40,
	CT_KEY_RIGHTPAREN = 41,
	CT_KEY_ASTERISK = 42,
	CT_KEY_PLUS = 43,
	CT_KEY_COMMA = 44,
	CT_KEY_MINUS = 45,
	CT_KEY_PERIOD = 46,
	CT_KEY_SLASH = 47,
	CT_KEY_0 = 48,
	CT_KEY_1 = 49,
	CT_KEY_2 = 50,
	CT_KEY_3 = 51,
	CT_KEY_4 = 52,
	CT_KEY_5 = 53,
	CT_KEY_6 = 54,
	CT_KEY_7 = 55,
	CT_KEY_8 = 56,
	CT_KEY_9 = 57,
	CT_KEY_COLON = 58,
	CT_KEY_SEMICOLON = 59,
	CT_KEY_LESS = 60,
	CT_KEY_EQUALS = 61,
	CT_KEY_GREATER = 62,
	CT_KEY_QUESTION = 63,
	CT_KEY_AT = 64,
	CT_KEY_LEFTBRACKET = 91,
	CT_KEY_BACKSLASH = 92,
	CT_KEY_RIGHTBRACKET = 93,
	CT_KEY_CARET = 94,
	CT_KEY_UNDERSCORE = 95,
	CT_KEY_BACKQUOTE = 96,
	CT_KEY_A = 97,
	CT_KEY_B = 98,
	CT_KEY_C = 99,
	CT_KEY_D = 100,
	CT_KEY_E = 101,
	CT_KEY_F = 102,
	CT_KEY_G = 103,
	CT_KEY_H = 104,
	CT_KEY_I = 105,
	CT_KEY_J = 106,
	CT_KEY_K = 107,
	CT_KEY_L = 108,
	CT_KEY_M = 109,
	CT_KEY_N = 110,
	CT_KEY_O = 111,
	CT_KEY_P = 112,
	CT_KEY_Q = 113,
	CT_KEY_R = 114,
	CT_KEY_S = 115,
	CT_KEY_T = 116,
	CT_KEY_U = 117,
	CT_KEY_V = 118,
	CT_KEY_W = 119,
	CT_KEY_X = 120,
	CT_KEY_Y = 121,
	CT_KEY_Z = 122,
	CT_KEY_DELETE = 127,
	CT_KEY_KP0 = 256,
	CT_KEY_KP1 = 257,
	CT_KEY_KP2 = 258,
	CT_KEY_KP3 = 259,
	CT_KEY_KP4 = 260,
	CT_KEY_KP5 = 261,
	CT_KEY_KP6 = 262,
	CT_KEY_KP7 = 263,
	CT_KEY_KP8 = 264,
	CT_KEY_KP9 = 265,
	CT_KEY_KP_PERIOD = 266,
	CT_KEY_KP_DIVIDE = 267,
	CT_KEY_KP_MULTIPLY = 268,
	CT_KEY_KP_MINUS = 269,
	CT_KEY_KP_PLUS = 270,
	CT_KEY_KP_ENTER = 271,
	CT_KEY_KP_EQUALS = 272,
	CT_KEY_UP = 273,
	CT_KEY_DOWN = 274,
	CT_KEY_RIGHT = 275,
	CT_KEY_LEFT = 276,
	CT_KEY_INSERT = 277,
	CT_KEY_HOME = 278,
	CT_KEY_END = 279,
	CT_KEY_PAGEUP = 280,
	CT_KEY_PAGEDOWN = 281,
	CT_KEY_F1 = 282,
	CT_KEY_F2 = 283,
	CT_KEY_F3 = 284,
	CT_KEY_F4 = 285,
	CT_KEY_F5 = 286,
	CT_KEY_F6 = 287,
	CT_KEY_F7 = 288,
	CT_KEY_F8 = 289,
	CT_KEY_F9 = 290,
	CT_KEY_F10 = 291,
	CT_KEY_F11 = 292,
	CT_KEY_F12 = 293,
	CT_KEY_F13 = 294,
	CT_KEY_F14 = 295,
	CT_KEY_F15 = 296,
	CT_KEY_NUMLOCK = 300,
	CT_KEY_CAPSLOCK = 301,
	CT_KEY_SCROLLOCK = 302,
	CT_KEY_RSHIFT = 303,
	CT_KEY_LSHIFT = 304,
	CT_KEY_RCTRL = 305,
	CT_KEY_LCTRL = 306,
	CT_KEY_RALT = 307,
	CT_KEY_LALT = 308,
	CT_KEY_RMETA = 309,
	CT_KEY_LMETA = 310,
	CT_KEY_LSUPER = 311,
	CT_KEY_RSUPER = 312,
	CT_KEY_MODE = 313,
	CT_KEY_COMPOSE = 314,
	CT_KEY_HELP = 315,
	CT_KEY_PRINT = 316,
	CT_KEY_SYSREQ = 317,
	CT_KEY_BREAK = 318,
	CT_KEY_MENU = 319,
	CT_KEY_POWER = 320,
	CT_KEY_EURO = 321,
	CT_KEY_UNDO = 322,
	CT_BUTTON_LEFT = 1,
	CT_BUTTON_MIDDLE = 2,
	CT_BUTTON_RIGHT = 3,
	CT_BUTTON_WHEELUP = 4,
	CT_BUTTON_WHEELDOWN = 5,
	CT_BUTTON_X1 = 6,
	CT_BUTTON_X2 = 7
} CT_Key;

#endif /* __core_h__ */	
