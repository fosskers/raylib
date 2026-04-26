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
