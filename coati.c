#include "coati.h"
#include <string.h>
#include <assert.h>
#include <math.h>
#include <hypermath.h>
#include <dynvector.h>
#include <GL/glew.h>
#include <GL/glu.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>

/* Constants */

static float colour_white[4] = { 1, 1, 1, 1 };

/* Utils */

#ifdef DEBUG
#define CHECK_GL() check_gl_error(__func__);
#else
#define CHECK_GL()
#endif

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

static void swap_float(float* a, float* b)
{
	float t = *a;
	*a = *b;
	*b = t;
}

static int zeroish(float v)
{
	return (v < .0001 && v > -.0001);
}

#ifdef DEBUG
static void check_gl_error(const char* func)
{
	GLuint err = glGetError();
	if (err != GL_NO_ERROR)
	{
		fprintf(stderr, "%s: OpenGL error: %s\n", func, gluErrorString(err));
		exit(-1);
	}
}
#endif

/* Error */

const char* ct_get_error()
{
	return SDL_GetError();
}

void ct_set_error(const char* str)
{
	SDL_SetError(str);
}

/* Shader */

typedef struct
{
	unsigned gl_program_id;
	unsigned gl_vertex_id;
	unsigned gl_fragment_id;
} CT_Shader;

const char* vertex_shader_source = 
	"#version 330\n"
	"layout (location = 0) in vec2 vertex; "
	"layout (location = 1) in vec2 coord; "
	"out vec4 f_colour; "
	"out vec2 f_coord; "
	"uniform mat4 modelview; "
	"uniform mat4 projection; "
	"uniform vec4 colour; "
	"void main() { "
		"gl_Position = projection * modelview * vec4(vertex, 0, 1); "
		"f_coord = coord; "
		"f_colour = colour; "
	"}";

const char* fragment_shader_source =
	"#version 330\n"
	"uniform sampler2D texture; "
	"in vec4 f_colour; "
	"in vec2 f_coord; "
	"out vec4 fragment; "
	"void main() { "
		"fragment = texture2D(texture, f_coord.st) * f_colour; "
	"}";

static GLuint compile_shader(const char* source, GLuint type, int* success)
{
	GLuint shader = glCreateShader(type);
	glShaderSource(shader, 1, (const GLchar**)&source, 0);
	glCompileShader(shader);
	glGetShaderiv(shader, GL_COMPILE_STATUS, success);
	if (*success == GL_FALSE)
	{
	        GLint log_length = 0;
	        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);
		char log_string[log_length];
		glGetShaderInfoLog(shader, log_length, &log_length, log_string);
		fprintf(stderr, "GLSL error: %s\n", log_string); 
		glDeleteShader(shader);
		*success = 0;
		return 0;
 	}
	CHECK_GL();
	return shader;
}

static GLuint create_shader_program(GLuint vertex, GLuint fragment, int* success)
{
	GLuint prog = glCreateProgram();
	glAttachShader(prog, vertex);
	glAttachShader(prog, fragment);
	glLinkProgram(prog);
	glGetProgramiv(prog, GL_LINK_STATUS, success);
	if (*success == GL_FALSE)
	{
		ct_set_error("Could not create shader program.");
		glDeleteProgram(prog);
		*success = 0;
		return 0;
	}
	glUseProgram(prog);
	glActiveTexture(GL_TEXTURE0);
	CHECK_GL();
	return prog;
}

static void shader_upload_colour(CT_Shader* shader, float* colour)
{
	glUniform4fv(glGetUniformLocation(shader->gl_program_id, "colour"),
		     1, colour);
	CHECK_GL();
}

static void shader_upload_modelview_matrix(CT_Shader* shader, float* matrix)
{
	glUniformMatrix4fv(glGetUniformLocation(shader->gl_program_id, "modelview"),
			   1, GL_FALSE, matrix);
	CHECK_GL();
}

static void shader_upload_projection_matrix(CT_Shader* shader, float* matrix)
{
	glUniformMatrix4fv(glGetUniformLocation(shader->gl_program_id, "projection"),
			   1, GL_FALSE, matrix);
	CHECK_GL();
}

static CT_Shader* ct_shader_create(const char* vertex_source,
		      const char* fragment_source)
{
	int success;
	GLuint vertex   = compile_shader(vertex_source,   GL_VERTEX_SHADER,   &success);
	if (!success) return NULL;
	GLuint fragment = compile_shader(fragment_source, GL_FRAGMENT_SHADER, &success);
	if (!success) return NULL;
	GLuint program  = create_shader_program(vertex, fragment, &success);
	if (!success) return NULL;

	CT_Shader* shader = smalloc(sizeof(CT_Shader));
	shader->gl_vertex_id   = vertex;
	shader->gl_fragment_id = fragment;
	shader->gl_program_id  = program;
	CHECK_GL();
	return shader;
}

static void ct_shader_free(CT_Shader* shader)
{
	free(shader);
}

static CT_Shader* _default_shader;

static CT_Shader* default_shader()
{
	return _default_shader;
}

/* Window */

static CT_Window window = { NULL, "Coati", 0, 1 };

static CT_Texture _ct_screen_texture;

int ct_window_init()
{
	/* Initialise SDL */
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) return 1;

	/* Open OpenGL context */
	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
//	glEnable(GL_BLEND);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	window.sdl_window = SDL_CreateWindow(window.title, 
					     SDL_WINDOWPOS_UNDEFINED,
					     SDL_WINDOWPOS_UNDEFINED,
					     800, 600,
					     SDL_WINDOW_OPENGL);
	SDL_GL_CreateContext(window.sdl_window);

	/*  Initialise Glew */
	GLint err = glewInit();
	if (err != GLEW_OK)
	{
		char str[1024];
		sprintf(str, "%s\n", glewGetErrorString(err));
		ct_set_error(str);
		return 1;
	}

	/* Initialise Default Shader */
	_default_shader = ct_shader_create(vertex_shader_source, fragment_shader_source);
	shader_upload_colour(default_shader(), colour_white);
	CHECK_GL();
	return 0;
}

void ct_window_quit()
{
	SDL_DestroyWindow(window.sdl_window);
	ct_shader_free(default_shader());
	SDL_Quit();
}

void ct_window_res_set(int x, int y)
{
	window.is_size_changed = 1;
	SDL_SetWindowSize(window.sdl_window, x, y);
}

void ct_window_fullscreen_set(int fullscreen)
{
	SDL_SetWindowFullscreen(window.sdl_window, fullscreen ? SDL_WINDOW_FULLSCREEN : 0);
	window.fullscreen = fullscreen;
}

int window_is_fullscreen()
{
	return window.fullscreen;
}

CT_Texture* ct_screen_texture()
{
	if (window.is_size_changed) {
		SDL_GetWindowSize(window.sdl_window,
				  &_ct_screen_texture.w,
				  &_ct_screen_texture.h);
	}
	return &_ct_screen_texture;
}

void ct_window_update()
{
	SDL_GL_SwapWindow(window.sdl_window);
}

/* Image */

static CT_Image* image_alloc(SDL_Surface* sur)
{
	CT_Image* image = smalloc(sizeof(CT_Image));
	image->sdl_surface = sur;
	return image;
}

CT_Image* ct_image_load(const char* filename)
{
	SDL_Surface* sur = IMG_Load(filename);
	if (sur == NULL)
	{
		char str[1024];
		sprintf(str, "Could not load file: %s", filename);
		ct_set_error(str);
		return NULL;
	}
	return image_alloc(sur);
}

CT_Image* ct_image_create(unsigned w, unsigned h)
{
	SDL_Surface* sur = SDL_CreateRGBSurface(0, w, h, 32, 0, 0, 0, 0);
	if (sur == NULL)
	{
		fprintf(stderr, "Error could not create image of size %d, %d.\n", w, h);
		return NULL;
	}
	return image_alloc(sur);
}

void ct_image_free(CT_Image* image)
{
	SDL_FreeSurface(image->sdl_surface);
	free(image);
}

unsigned ct_image_bpp(CT_Image* image)
{
	return image->sdl_surface->format->BytesPerPixel;
}

static unsigned ct_image_gl_format(CT_Image* image)
{
	switch(ct_image_bpp(image))
	{
	case 4: return SDL_BYTEORDER == SDL_BIG_ENDIAN ? GL_BGRA : GL_RGBA;
	case 3: return SDL_BYTEORDER == SDL_BIG_ENDIAN ? GL_BGR  : GL_RGB;
	}
	assert(0);
}

void ct_image_size(CT_Image* image, float* vect)
{
	vect[0] = (float)image->sdl_surface->w;
	vect[1] = (float)image->sdl_surface->h;
}

/* Colour */

static struct
{
	float stack[CT_MAX_COLOUR_STACK_SIZE*4];
	unsigned size;
} colour_stack;

void ct_push_colour(float* colour)
{
	if (colour_stack.size >= CT_MAX_COLOUR_STACK_SIZE)
	{
		/* Stack overflow, resetting stack to prevent
		   crashing if this error is ignored. */
		ct_set_error("Stack overflow");
		colour_stack.size = 0;
	}
	memcpy(colour_stack.stack+(colour_stack.size*4),
	       colour,
	       sizeof(float)*4);
	colour_stack.size++;
	shader_upload_colour(default_shader(), colour);
}

void ct_pop_colour()
{
	if (colour_stack.size == 0)
	{
		/* Stack underflow, setting stack to one
		   so it will set to default value if this
		   error is ignored. */
		ct_set_error("Stack underflow");
		colour_stack.size = 1;
	}
	colour_stack.size--;
	if (colour_stack.size == 0)
	{
		shader_upload_colour(default_shader(), colour_white);
	} else
	{
		shader_upload_colour(default_shader(), colour_stack.stack+(colour_stack.size*4));
	}
}

/* Blending */

static struct
{
	CT_BlendMode stack[CT_MAX_BLEND_STACK_SIZE];
	unsigned size;
} blend_stack;

static void set_blend_mode(CT_BlendMode mode)
{
	if (blend_stack.size == 0)
	{
		glEnable(GL_BLEND);
	}
	switch(mode)
	{
	case CT_BLEND_MODE_NORMAL:
		glBlendFunc(GL_ONE, GL_ZERO);
		break;
	case CT_BLEND_MODE_TRANS:
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		break;
	case CT_BLEND_MODE_ADD:
		glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
		break;
	case CT_BLEND_MODE_ONE_ONE:
		glBlendFunc(GL_ONE, GL_ZERO);
		break;
	}
}

void ct_push_blend_mode(CT_BlendMode mode)
{
	if (blend_stack.size >= CT_MAX_BLEND_STACK_SIZE)
	{
		/* Stack overflow, resetting stack to prevent
		   crashing if this error is ignored. */
		ct_set_error("Stack overflow");
		colour_stack.size = 0;
	}
	blend_stack.stack[blend_stack.size++] = mode;
	glEnable(GL_BLEND);
	set_blend_mode(mode);
}

void ct_pop_blend_mode()
{
	if (blend_stack.size == 0)
	{
		/* Stack underflow, setting stack to one
		   so it will set to default value if this
		   error is ignored. */
		ct_set_error("Stack underflow");
		blend_stack.size = 1;
	}
	blend_stack.size--;
	if (blend_stack.size == 0)
	{
		set_blend_mode(CT_BLEND_MODE_NORMAL);
	} else {
		set_blend_mode(blend_stack.stack[blend_stack.size]);
	}

}

/* Texture */

static CT_Texture* current_target();

static GLuint create_buffer(GLuint tex_id)
{
	GLuint buf_id; glGenFramebuffers(1, &buf_id);
	glBindFramebuffer(GL_FRAMEBUFFER, buf_id);
	glFramebufferTexture2D(GL_FRAMEBUFFER,
			       GL_COLOR_ATTACHMENT0,
			       GL_TEXTURE_2D, tex_id, 0);
	glBindFramebuffer(GL_FRAMEBUFFER, current_target()->gl_buffer_id);
	CHECK_GL();
	return buf_id;
}

static CT_Texture* new_texture(unsigned w, unsigned h)
{
	GLuint tex_id; glGenTextures(1, &tex_id);
	CT_Texture* tex = smalloc(sizeof(CT_Texture));
	tex->w = w;
	tex->h = h;
	/**/
	glBindTexture(GL_TEXTURE_2D, tex_id);
	/* Use repeat for wrapping-mode */
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);	
	/* Don't use mipmapping */
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	/**/
	tex->gl_texture_id = tex_id;
	tex->gl_buffer_id  = create_buffer(tex_id);
	CHECK_GL();
	return tex;
}

static CT_Texture* texture_init(CT_Image* image, unsigned format)
{
	void* pixels = image->sdl_surface->pixels;
	unsigned w = image->sdl_surface->w;
	unsigned h = image->sdl_surface->h;
	CT_Texture* tex = new_texture(w, h);
	glBindTexture(GL_TEXTURE_2D, tex->gl_texture_id);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,
		     w, h,
		     0, format, GL_UNSIGNED_BYTE, pixels);
	return tex;
}

CT_Texture* ct_texture_from_image(CT_Image* image)
{
	return texture_init(image, ct_image_gl_format(image));
}

CT_Texture* ct_texture_create(unsigned w, unsigned h)
{
	CT_Image* image = ct_image_create(w, h);
	CT_Texture* tex = ct_texture_from_image(image);
	if (!image) return NULL; /* ct_image_create already prints error message. */
	ct_image_free(image);
	return tex;
}

CT_Texture* ct_texture_load(const char* filename)
{
	CT_Image* image = ct_image_load(filename);
	if (!image) return NULL; /* ct_image_load already prints error message. */
	CT_Texture* tex = ct_texture_from_image(image);
	ct_image_free(image);
	return tex;
}

CT_Texture* texture_copy(CT_Texture* texture)
{
	CT_Texture* tex = ct_texture_create(texture->w, texture->h);
	/**/
	CT_Transformation trans = {
		{ 0, 1, 0, 1 },
		{ 0, 1, 0, 1 },
		{ 0, 0 }, 0,
		-1, -1 };
	float idem[16]; hpmIdentityMat4(idem);
	ct_push_blend_mode(CT_BLEND_MODE_NORMAL);
	ct_push_target(tex);
	ct_texture_render(texture, idem, &trans);
	ct_pop_target();
	ct_pop_blend_mode();
	return tex;
}

void ct_texture_free(CT_Texture* tex)
{
	glDeleteTextures(1, &tex->gl_texture_id);
	glDeleteBuffers(1, &tex->gl_buffer_id);
	CHECK_GL();
	free(tex);
}

int ct_is_texture_screen(CT_Texture* tex)
{
	return tex->gl_buffer_id == 0;
}

void ct_texture_size(CT_Texture* texture, float* vect)
{
	if (ct_is_texture_screen(texture)) {
	}
	vect[0] = (float)texture->w;
	vect[1] = (float)texture->h;
}

static float project_matrix[16];

static void texture_bind(CT_Texture* tex)
{
	glViewport(0, 0, tex->w, tex->h);
	hpmOrthoFloat(1, ct_is_texture_screen(tex) ? -1 : 1, -100, 100, project_matrix);
	/* Put origin origin at 0,0 */
	hpmTranslation(-.5, -.5, 0, project_matrix);
	hpmScale2D(2, ct_is_texture_screen(tex) ? -2 : 2, project_matrix);
	shader_upload_projection_matrix(default_shader(), project_matrix);
	glBindFramebuffer(GL_FRAMEBUFFER, tex->gl_buffer_id);
	CHECK_GL();
}


void ct_push_target(CT_Texture* tex);

void ct_pop_target();

void ct_texture_clear(CT_Texture* tex, float* colour)
{
	ct_push_target(tex);
	glClearBufferfv(GL_COLOR, 0, colour);
	ct_pop_target();
	CHECK_GL();
}

static GLushort rect_index_order[] = { 0, 1, 2, 0, 2, 3 };

void vertex_data(CT_Transformation* tran, float* data);

void ct_texture_render(CT_Texture* tex, float* matrix, CT_Transformation* trans)
{
	float data[16]; vertex_data(trans, data);
	glUseProgram(default_shader()->gl_program_id);
	glBindTexture(GL_TEXTURE_2D, tex->gl_texture_id);
	shader_upload_modelview_matrix(default_shader(), matrix);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 16, data);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 16, data+2);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, rect_index_order);
	CHECK_GL();
}

/* Target */

static struct
{
	CT_Texture* stack[CT_MAX_TARGET_STACK_SIZE];
	unsigned size;
} target_stack;

static CT_Texture* current_target()
{
	return target_stack.size
		? target_stack.stack[target_stack.size-1]
		: CT_SCREEN;
}

void ct_push_target(CT_Texture* tex)
{
	if (target_stack.size >= CT_MAX_TARGET_STACK_SIZE)
	{
		/* Stack overflow, resetting stack to prevent
		   crashing if this error is ignored. */
		ct_set_error("Stack overflow");
		target_stack.size = 0;
	}
	texture_bind(tex);
	target_stack.stack[target_stack.size++] = tex;
}

void ct_pop_target()
{
	if (target_stack.size == 0)
	{
		/* Stack underflow, setting stack to one
		   so it will set to default value if this
		   error is ignored. */
		ct_set_error("Stack underflow");
		target_stack.size = 1;
	}
	target_stack.size--;
	texture_bind(target_stack.size
		     ? target_stack.stack[target_stack.size-1]
		     : CT_SCREEN);
}

/* Batch */

CT_Batch* ct_batch_create(CT_Texture* atlas, unsigned size)
{
	CT_Batch* batch = smalloc(sizeof(CT_Batch));
	batch->vector  = dv_vector_new(16, size);
	batch->indices = smalloc(sizeof(unsigned short)*size*6);
	unsigned int i=0;
	for (i=0; i<size; i++) {
		batch->indices[(i*6)+0] = 0 + (i*4);
		batch->indices[(i*6)+1] = 1 + (i*4);
		batch->indices[(i*6)+2] = 2 + (i*4);
		batch->indices[(i*6)+3] = 0 + (i*4);
		batch->indices[(i*6)+4] = 2 + (i*4);
		batch->indices[(i*6)+5] = 3 + (i*4);
	}
	batch->atlas = atlas;
	return batch;
}

void ct_batch_free(CT_Batch* batch)
{
	dv_vector_free(batch->vector);
	free(batch->indices);
	free(batch);
}

unsigned ct_batch_push(CT_Batch* batch, CT_Transformation* trans)
{
	
	float data[16]; vertex_data(trans, data);
	return dv_vector_push(batch->vector, data);
}

void ct_batch_remove(CT_Batch* batch, unsigned id)
{
	dv_vector_remove(batch->vector, id);
}

void ct_batch_change(CT_Batch* batch, unsigned id, CT_Transformation* trans)
{
	vertex_data(trans, dv_vector_ref(batch->vector, id));
}

void ct_batch_render(CT_Batch* batch, float* matrix)
{
	DV_Vector* vector = batch->vector;
	glUseProgram(default_shader()->gl_program_id);
	glBindTexture(GL_TEXTURE_2D, batch->atlas->gl_texture_id);
	shader_upload_modelview_matrix(default_shader(), matrix);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 16, vector->data);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 16, vector->data+2);
	glDrawElements(GL_TRIANGLES, vector->size*6, GL_UNSIGNED_SHORT, batch->indices);
	CHECK_GL();
}

/* Font */

CT_Font* ct_font_load(const char* filename)
{
	FILE* file = fopen(filename, "r");
	if(!file)
	{
		char str[1024];
		sprintf(str, "%s: file not found.", filename);
		ct_set_error(str);
		return NULL;
	}
	SDL_RWops* rw = SDL_RWFromFP(file, SDL_TRUE);
	if (!rw)
	{
		char str[1024];
		sprintf(str, "could not load file %s.", filename);
		ct_set_error(str);
		return NULL;
	}
	CT_Font* font = smalloc(sizeof(CT_Font));
	font->file = file;
	font->rw = rw;
	font->first = NULL;
	return font;
}

void ct_font_free(CT_Font* font)
{
	SDL_FreeRW(font->rw);
	/* Free font map */
	struct CT_FontMapLink* last = font->first, *tmp;
	if (last)
	{
		while (last)
		{
			tmp = last;
			last = last->next;
			free(tmp);
		}
	}
	free(font);
}

static TTF_Font* load_font_size(CT_Font* font, unsigned size)
{
	if (!TTF_WasInit()) TTF_Init();
	TTF_Font* ttf_font = TTF_OpenFontRW(font->rw, 0, size);
	if (!ttf_font)
	{
		ct_set_error(TTF_GetError());
		return NULL;
	}
	struct CT_FontMapLink* link = smalloc(sizeof(struct CT_FontMapLink));
	link->size  = size;
	link->value = ttf_font;
	link->next  = NULL;
	struct CT_FontMapLink* last = font->first;
	while (last)
	{
		last = last->next;
	}

	last = smalloc(sizeof(struct CT_FontMapLink));
	last->next = link;
	return ttf_font;
}

static TTF_Font* get_ttf_font(CT_Font* font, unsigned size)
{
	struct CT_FontMapLink* last = font->first;
	TTF_Font* ttf_font = NULL;
	while (last)
	{
		if (last->size == size)
		{
			ttf_font = last->value;
			break;
		}	
		last = last->next;
	}
	return ttf_font ? ttf_font : load_font_size(font, size);
}

extern CT_Texture* ct_texture_from_string(CT_Font* font,
				    unsigned size,
				    const char* string,
				    float* colour)
{
	TTF_Font* ttf_font = get_ttf_font(font, size);
	if (!ttf_font) return NULL; /* Error already reported. */
	SDL_Color sdl_colour = { 255*colour[0],
				 255*colour[1],
				 255*colour[2],
				 255*colour[3] };
	int w, h;
	TTF_SizeText(ttf_font, string, &w, &h);
	SDL_Surface* sur = TTF_RenderText_Blended(ttf_font, string, sdl_colour);
	CT_Image* image = image_alloc(sur);
	CT_Texture* tex = ct_texture_from_image(image);
	ct_image_free(image);
	return tex;
}

/* Input */

typedef struct
{
	Uint8 stack[CT_MAX_INPUT_STACK_SIZE];
	Uint8 size;
} InputStack;

static InputStack pressed_stack, released_stack, holded_stack;

static int _ct_is_quitting = 0;

static int has_key(CT_Key key, InputStack* stack)
{
	unsigned i;
	for (i=0; i<CT_MAX_INPUT_STACK_SIZE; i++)
	{
		if (stack->stack[i] == key) return 1;
	}
	return 0;
}

static void push_key(CT_Key key, InputStack* stack)
{
	if (key && !has_key(key, stack))
	{
		if (stack->size == CT_MAX_INPUT_STACK_SIZE) stack->size = 0;
		stack->stack[stack->size++] = key;
	}
}

static void pop_key(CT_Key key, InputStack* stack)
{
	unsigned i;
	for (i=0; i<CT_MAX_INPUT_STACK_SIZE; i++)
	{
		if (stack->stack[i] == key)
		{
			stack->stack[i] = 0;
			return;
		}
	}
}

static void key_down_callback(Uint8 key)
{
	push_key(key, &pressed_stack);
}

static void key_up_callback(Uint8 key)
{

	unsigned i;
	for (i=0; i<CT_MAX_INPUT_STACK_SIZE; i++)
	{
	}
	pop_key (key, &holded_stack);
	push_key(key, &released_stack);
}

static void reset_stacks()
{
	unsigned i;
	for (i=0; i<CT_MAX_INPUT_STACK_SIZE; i++)
	{
		push_key(pressed_stack.stack[i], &holded_stack);
		pressed_stack.stack [i] = 0;
		released_stack.stack[i] = 0;
	}
}

int ct_is_quitting()
{
	return _ct_is_quitting;
}

int ct_is_key_pressed(CT_Key key)
{
	return has_key(key, &pressed_stack);
}

int ct_is_key_released(CT_Key key)
{
	return has_key(key, &released_stack);
}

int ct_is_key_holded(CT_Key key)
{
	return has_key(key, &holded_stack);
}

void ct_mouse_position(float* ret)
{
	int x, y;
	SDL_GetMouseState(&x, &y);
	ret[0] = x;
	ret[1] = y;
}

void ct_poll_input()
{
	reset_stacks();

	SDL_Event event;
	while(SDL_PollEvent(&event) == 1)
	{
		switch(event.type) {
		case SDL_MOUSEBUTTONDOWN:
			key_down_callback(event.button.button);
			break;
		case SDL_MOUSEBUTTONUP:
			key_up_callback(event.button.button);
			break;
		case SDL_KEYDOWN:
			key_down_callback(event.key.keysym.sym);
			break;
		case SDL_KEYUP:
			key_up_callback(event.key.keysym.sym);
			break;
		case SDL_QUIT:
			_ct_is_quitting = 1;
			break;
		}
	}
}

static const char* key_names[] = {
	"no_key",
	"button_left",
	"button_middle",
	"button_right",
	"button_wheelup",
	"button_wheeldown",
	"button_x1",
	"button_x2",
	"backspace",
	"tab",
	"unknown",
	"unknown",
	"clear",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"pause",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"escape",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"space",
	"exclaim",
	"quotedbl",
	"hash",
	"dollar",
	"unknown",
	"ampersand",
	"quote",
	"leftparen",
	"rightparen",
	"asterisk",
	"plus",
	"comma",
	"minus",
	"period",
	"slash",
	"0",
	"1",
	"2",
	"3",
	"4",
	"5",
	"6",
	"7",
	"8",
	"9",
	"colon",
	"semicolon",
	"less",
	"equals",
	"greater",
	"question",
	"at",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"leftbracket",
	"backslash",
	"rightbracket",
	"caret",
	"underscore",
	"backquote",
	"a",
	"b",
	"c",
	"d",
	"e",
	"f",
	"g",
	"h",
	"i",
	"j",
	"k",
	"l",
	"m",
	"n",
	"o",
	"p",
	"q",
	"r",
	"s",
	"t",
	"u",
	"v",
	"w",
	"x",
	"y",
	"z",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"delete",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"unknown",
	"kp0",
	"kp1",
	"kp2",
	"kp3",
	"kp4",
	"kp5",
	"kp6",
	"kp7",
	"kp8",
	"kp9",
	"keypad_period",
	"keypad_divide",
	"keypad_multiply",
	"keypad_minus",
	"keypad_plus",
	"keypad_enter",
	"keypad_equals",
	"up",
	"down",
	"right",
	"left",
	"insert",
	"home",
	"end",
	"pageup",
	"pagedown",
	"f1",
	"f2",
	"f3",
	"f4",
	"f5",
	"f6",
	"f7",
	"f8",
	"f9",
	"f10",
	"f11",
	"f12",
	"f13",
	"f14",
	"f15",
	"unknown",
	"unknown",
	"unknown",
	"numlock",
	"capslock",
	"scrollock",
	"right shift",
	"left shift",
	"right ctrl",
	"left ctrl",
	"right alt",
	"left alt",
	"right meta",
	"left meta",
	"left super",
	"right super",
	"mode",
	"compose",
	"help",
	"print",
	"sysreq",
	"break",
	"menu",
	"power",
	"euro",
	"undo"
};

const char* ct_key_name(CT_Key key)
{
	return key_names[key];
}

/* Transformation */

void vertex_data(CT_Transformation* tran, float* data)
{
	float l1 = tran->dst_rect[0];
	float r1 = tran->dst_rect[1];
	float t1 = tran->dst_rect[2];
	float b1 = tran->dst_rect[3];
	/**/
	float l2 = tran->src_rect[0];
	float r2 = tran->src_rect[1];
	float t2 = tran->src_rect[2];
	float b2 = tran->src_rect[3];
	/**/
	float px = tran->origin[0];
	float py = tran->origin[1];
	/**/
	if (tran->flip_h > 0) swap_float(&r2, &l2);
	if (tran->flip_v > 0) swap_float(&b2, &t2);
	/**/
	if (zeroish(tran->rotation))
	{
		float new_data[] = {
			l1-px, t1-py, l2, t2,
			r1-px, t1-py, r2, t2,
			r1-px, b1-py, r2, b2,
			l1-px, b1-py, l2, b2 };
		memcpy(data, new_data, sizeof(float)*16);
	} else
	{
		float ca = cos(tran->rotation);
		float sa = sin(tran->rotation);
		/**/
		float x1 = ((l1 - px) * ca) - ((t1 - py) * sa);
		float y1 = ((l1 - px) * sa) + ((t1 - py) * ca);
		float x2 = ((r1 - px) * ca) - ((t1 - py) * sa);
		float y2 = ((r1 - px) * sa) + ((t1 - py) * ca);
		float x3 = ((r1 - px) * ca) - ((b1 - py) * sa);
		float y3 = ((r1 - px) * sa) + ((b1 - py) * ca);
		float x4 = ((l1 - px) * ca) - ((b1 - py) * sa);
		float y4 = ((l1 - px) * sa) + ((b1 - py) * ca);
		float new_data[] = {
			x1+px, y1+py, l2, t2,
			x2+px, y2+py, r2, t2,
			x3+px, y3+py, r2, b2,
			x4+px, y4+py, l2, b2 };
		memcpy(data, new_data, sizeof(float)*16);
	}
}
