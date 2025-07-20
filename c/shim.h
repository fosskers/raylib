#ifndef SHIM_H_
#define SHIM_H_

#include "raylib.h"
#include <stdbool.h>

// --- Vectors --- //

Vector2 *_MakeVector2(float x, float y);

// --- Rectangles --- //

Rectangle *_MakeRectangle(float x, float y, float width, float height);

// --- Colours --- //

Color *_MakeColor(unsigned char r, unsigned char g, unsigned char b,
                  unsigned char a);

Color *_ColorAlpha(Color *color, float alpha);

// --- Textures --- //

Texture2D *_LoadTexture(const char *fileName);

void _UnloadTexture(Texture2D *texture);

bool _IsTextureValid(Texture2D *texture);

void _DrawTexture(Texture2D *texture, int posX, int posY, Color *tint);

void _DrawTextureV(Texture2D *texture, Vector2 *position, Color *tint);

void _DrawTextureRec(Texture2D *texture, Rectangle *source, Vector2 *position,
                     Color *tint);

// --- Sounds and Music --- //

Sound *_LoadSound(const char *fileName);

void _UnloadSound(Sound *sound);

void _PlaySound(Sound *sound);

Music *_LoadMusicStream(const char *fileName);

void _UnloadMusicStream(Music *music);

bool _IsMusicStreamPlaying(Music *music);

void _PlayMusicStream(Music *music);

void _UpdateMusicStream(Music *music);

// --- Camera --- //

Camera2D *_MakeCamera2D(Vector2 *offset, Vector2 *target, float rotation,
                        float zoom);

// --- Window --- //

void _ClearBackground(Color *color);

void _BeginMode2D(Camera2D *camera);

void _DrawText(const char *text, int posX, int posY, int fontSize,
               Color *color);

void _DrawRectangle(int posX, int posY, int width, int height, Color *color);

void _DrawLine(int startPosX, int startPosY, int endPosX, int endPosY,
               Color *color);

void _DrawPixel(int posX, int posY, Color *color);

// --- Collision --- //

bool _CheckCollisionRecs(Rectangle *rec1, Rectangle *rec2);

#endif // SHIM_H_
