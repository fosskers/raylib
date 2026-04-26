#ifndef SHIM_H_
#define SHIM_H_

#include "raylib.h"
#include <stdbool.h>

// --- Vectors --- //

Vector2 _MakeVector2(float x, float y);

// --- Rectangles --- //

Rectangle _MakeRectangle(float x, float y, float width, float height);

// --- Colours --- //

Color _MakeColor(unsigned char r, unsigned char g, unsigned char b,
                 unsigned char a);

// --- Camera --- //

Camera2D _MakeCamera2D(Vector2 offset, Vector2 target, float rotation,
                       float zoom);

#endif // SHIM_H_
