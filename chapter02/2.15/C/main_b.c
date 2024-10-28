#include "raylib.h"

/*

Setup: download the latest raylib release

cd ~/dl
echo Keep only the "include" and "lib" directories
sudo cp -r * /usr/

---

Compile with (all warnings):

gcc -Wall -o /tmp/tmp main_b.c -l raylib

 */

#include "raylib.h"

const int size = 800;

int main(void) {
  InitWindow(size, size, "PI Approximation");
  // SetTargetFPS(120);

  int unsigned inside = 0;
  int unsigned outside = 0;
  int unsigned total = 0;
  float piApprox = 0.0f;

  RenderTexture2D texture = LoadRenderTexture(size, size);

  while (!WindowShouldClose()) {
    int x = GetRandomValue(0, size);
    int y = GetRandomValue(0, size);

    total++;
    int isInside = (x * x + y * y <= size * size) ? 1 : 0;
    if (isInside) {
      inside++;
    } else {
      outside++;
    }
    piApprox = (float)inside / (float)total * 4;

    BeginTextureMode(texture);

    // Draw point
    if (isInside) {
      DrawPixel(x, y, BLUE);
    } else {
      DrawPixel(x, y, RED);
    }
    EndTextureMode();

    BeginDrawing();
    ClearBackground(BLACK);
    DrawTexture(texture.texture, 0, 0, WHITE);

    // Draw PI
    DrawRectangle(0, 0, 145, 60, BLACK);
    DrawText(TextFormat("PI = %.5f", piApprox), 10, 10, 20, WHITE);
    DrawText(TextFormat("%d", total), 10, 30, 20, WHITE);

    EndDrawing();
  }

  // Cleanup
  UnloadRenderTexture(texture);
  CloseWindow();
  return 0;
}
