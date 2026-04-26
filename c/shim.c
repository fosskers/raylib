#include "raylib.h"
#include <stdbool.h>

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

// --- Camera --- //

Camera2D _MakeCamera2D(Vector2 offset, Vector2 target, float rotation,
                       float zoom) {

    Camera2D camera = {offset, target, rotation, zoom};
    return camera;
}
