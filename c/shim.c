#include "raylib.h"
#include <stdbool.h>
#include <stdlib.h>

// --- Vectors --- //

Vector2 _MakeVector2(float x, float y) {
    Vector2 v = {x, y};
    return v;
}

// --- Rectangles --- //

Rectangle _MakeRectangle(float x, float y, float width, float height) {
    Rectangle r = {x, y, width, height};
    return r;
}

// --- Colours --- //

Color _MakeColor(unsigned char r, unsigned char g, unsigned char b,
                 unsigned char a) {
    Color c = {r, g, b, a};
    return c;
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

Font *_GetFontDefault(void) {
    Font *heap = malloc(sizeof(Font));
    Font stack = GetFontDefault();
    *heap = stack;

    return heap;
}

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
