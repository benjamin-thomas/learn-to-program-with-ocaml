#include <SDL2/SDL.h>
#include <time.h>

/*

apt install libsdl2-dev

---

Compile with (all warnings):

gcc -Wall -o /tmp/tmp ./main_c_sdl2.c -l SDL2 && /tmp/tmp

 */

const int size = 800;

// Generate a random integer between min and max (inclusive)
int rand_val(int min, int max) { return min + rand() % (max - min + 1); }

int main(void) {
  srand(time(NULL));

  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
    return -1;
  }

  SDL_Window *window =
      SDL_CreateWindow("PI Approximation", SDL_WINDOWPOS_UNDEFINED,
                       SDL_WINDOWPOS_UNDEFINED, size, size, SDL_WINDOW_SHOWN);

  if (!window) {
    printf("Window could not be created! SDL_Error: %s\n", SDL_GetError());
    SDL_Quit();
    return -1;
  }

  SDL_Renderer *renderer =
      SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) {
    printf("Renderer could not be created! SDL Error: %s\n", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return -1;
  }

  // Create a persistent texture for drawing points
  SDL_Texture *texture = SDL_CreateTexture(
      renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, size, size);
  if (!texture) {
    printf("Texture could not be created! SDL Error: %s\n", SDL_GetError());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return -1;
  }

  // Clear the texture initially
  SDL_SetRenderTarget(renderer, texture);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  SDL_RenderClear(renderer);

  // Variables for PI approximation
  unsigned int inside = 0;
  unsigned int total = 0;
  float piApprox = 0.0f;

  int running = 1;
  while (running) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
        running = 0;
      }
    }

    // Generate random point
    int x = rand_val(0, size);
    int y = rand_val(0, size);

    // Update PI approximation
    total++;
    int isInside = (x * x + y * y <= size * size) ? 1 : 0;
    if (isInside)
      inside++;
    piApprox = (float)inside / (float)total * 4.0f;

    // Draw point
    if (isInside) {
      SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
    } else {
      SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
    }
    SDL_RenderDrawPoint(renderer, x, y);

    // Reset to the main rendering target and draw the texture
    SDL_SetRenderTarget(renderer, NULL);
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, NULL, NULL);

    // Print PI value and total points in the console (for debugging)
    printf("\rPI = %.5f | Total Points: %d", piApprox, total);

    SDL_RenderPresent(renderer);

    // Set the renderer target back to the texture for the next point
    SDL_SetRenderTarget(renderer, texture);
  }

  // Cleanup
  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
