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
