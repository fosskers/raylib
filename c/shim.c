#include "raylib.h"
#include <stdbool.h>
#include <stdlib.h>

// --- Vectors --- //

Vector2 *_MakeVector2(float x, float y) {
  Vector2 *v = malloc(sizeof(Vector2));

  v->x = x;
  v->y = y;

  return v;
}

// --- Rectangles --- //

Rectangle *_MakeRectangle(float x, float y, float width, float height) {
  Rectangle *r = malloc(sizeof(Rectangle));

  r->x = x;
  r->y = y;
  r->width = width;
  r->height = height;

  return r;
}

// --- Colours --- //

Color *_MakeColor(unsigned char r, unsigned char g, unsigned char b,
                  unsigned char a) {
  Color *c = malloc(sizeof(Color));

  c->r = r;
  c->g = g;
  c->b = b;
  c->a = a;

  return c;
}

Color *_ColorAlpha(Color *color, float alpha) {
  Color stack = *color;
  Color new = ColorAlpha(stack, alpha);
  Color *heap = malloc(sizeof(Color));
  *heap = new;

  return heap;
}

// --- Textures --- //

Texture2D *_LoadTexture(const char *fileName) {
  Texture2D *heap = malloc(sizeof(Texture2D));
  Texture2D stack = LoadTexture(fileName);
  *heap = stack;

  return heap;
}

void _UnloadTexture(Texture2D *texture) {
  Texture2D stack = *texture;
  UnloadTexture(stack);
}

bool _IsTextureValid(Texture2D *texture) {
  Texture2D stack = *texture;
  return IsTextureValid(stack);
}

void _DrawTexture(Texture2D *texture, int posX, int posY, Color *tint) {
  Texture2D t = *texture;
  Color c = *tint;
  DrawTexture(t, posX, posY, c);
}

void _DrawTextureV(Texture2D *texture, Vector2 *position, Color *tint) {
  Texture2D t = *texture;
  Vector2 v = *position;
  Color c = *tint;
  DrawTextureV(t, v, c);
}

void _DrawTextureRec(Texture2D *texture, Rectangle *source, Vector2 *position,
                     Color *tint) {
  Texture2D t = *texture;
  Rectangle r = *source;
  Vector2 v = *position;
  Color c = *tint;
  DrawTextureRec(t, r, v, c);
}

// --- Sounds and Music --- //

Sound *_LoadSound(const char *fileName) {
  Sound stack = LoadSound(fileName);
  Sound *heap = malloc(sizeof(Sound));
  *heap = stack;
  return heap;
}

void _UnloadSound(Sound *sound) {
  Sound stack = *sound;
  UnloadSound(stack);
}

void _PlaySound(Sound *sound) {
  Sound stack = *sound;
  PlaySound(stack);
}

Music *_LoadMusicStream(const char *fileName) {
  Music stack = LoadMusicStream(fileName);
  Music *heap = malloc(sizeof(Music));
  *heap = stack;
  return heap;
}

void _UnloadMusicStream(Music *music) {
  Music stack = *music;
  UnloadMusicStream(stack);
}

bool _IsMusicStreamPlaying(Music *music) {
  Music stack = *music;
  return IsMusicStreamPlaying(stack);
}

void _PlayMusicStream(Music *music) {
  Music stack = *music;
  PlayMusicStream(stack);
}

void _UpdateMusicStream(Music *music) {
  Music stack = *music;
  UpdateMusicStream(stack);
}

// --- Camera --- //

Camera2D *_MakeCamera2D(Vector2 *offset, Vector2 *target, float rotation,
                        float zoom) {
  Camera2D *camera = malloc(sizeof(Camera2D));
  Vector2 o = *offset;
  Vector2 t = *target;

  camera->offset = o;
  camera->target = t;
  camera->rotation = rotation;
  camera->zoom = zoom;

  return camera;
}

Vector2 *_GetWorldToScreen2D(Vector2 *position, Camera2D *camera) {
  Vector2 *heap = malloc(sizeof(Vector2));
  Vector2 stack = GetWorldToScreen2D(*position, *camera);

  heap->x = stack.x;
  heap->y = stack.y;

  return heap;
}

// --- Window --- //

void _ClearBackground(Color *color) {
  Color stack = *color;
  ClearBackground(stack);
}

void _BeginMode2D(Camera2D *camera) {
  Camera2D stack = *camera;
  BeginMode2D(stack);
}

void _DrawText(const char *text, int posX, int posY, int fontSize,
               Color *color) {
  Color stack = *color;
  DrawText(text, posX, posY, fontSize, stack);
}

void _DrawTextEx(Font *font, const char *text, Vector2 *position,
                 float fontSize, float spacing, Color *tint) {
  Font f_stack = *font;
  Vector2 v_stack = *position;
  Color c_stack = *tint;
  DrawTextEx(f_stack, text, v_stack, fontSize, spacing, c_stack);
}

void _DrawRectangle(int posX, int posY, int width, int height, Color *color) {
  Color stack = *color;
  DrawRectangle(posX, posY, width, height, stack);
}

void _DrawLine(int startPosX, int startPosY, int endPosX, int endPosY,
               Color *color) {
  Color stack = *color;
  DrawLine(startPosX, startPosY, endPosX, endPosY, stack);
}

void _DrawPixel(int posX, int posY, Color *color) {
  Color stack = *color;
  DrawPixel(posX, posY, stack);
}

// --- Collision --- //

bool _CheckCollisionRecs(Rectangle *rec1, Rectangle *rec2) {
  Rectangle a = *rec1;
  Rectangle b = *rec2;
  return CheckCollisionRecs(a, b);
}

bool _CheckCollisionPointRec(Vector2 *point, Rectangle *rec) {
  Vector2 a = *point;
  Rectangle b = *rec;
  return CheckCollisionPointRec(a, b);
}

// --- Fonts --- //

Font *_LoadFont(const char *fileName) {
  Font *heap = malloc(sizeof(Font));
  Font stack = LoadFont(fileName);
  *heap = stack;

  return heap;
}

Font *_LoadFontEx(const char *fileName, int fontSize, int *codepoints,
                  int codepointCount) {
  Font *heap = malloc(sizeof(Font));
  Font stack = LoadFontEx(fileName, fontSize, codepoints, codepointCount);
  *heap = stack;

  return heap;
}

void _UnloadFont(Font *font) {
  Font stack = *font;
  UnloadFont(stack);
}

bool _IsFontValid(Font *font) {
  Font stack = *font;
  return IsFontValid(stack);
}
