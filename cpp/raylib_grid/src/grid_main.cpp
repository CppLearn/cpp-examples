#include <cstdlib>
#include <sstream>

#include "raylib.h"
#include "square.hpp"
#include "grid.hpp"

// global for paused state.
bool paused = false;

//  ---------------------------------------------------------------  //
//  Timing-related functions                                         //
//  ---------------------------------------------------------------  //

/*
void SetTargetFPS(int fps);                                 // Set target FPS (maximum)
int GetFPS(void);                                           // Get current FPS
float GetFrameTime(void);                                   // Get time in seconds for last frame drawn (delta time)
double GetTime(void);                                       // Get elapsed time in seconds since InitWindow()
*/

int main(int argc, char *argv[])
{
	const int screen_width  = 800;
	const int screen_height = 800;

	const int grid_width  = 500;
	const int grid_height = 500;

	const int num_squares = 40;
	
	InitWindow(screen_width, screen_height, "Grid");
	const int fps = 60;
	SetTargetFPS(fps); // Set our app to run at 60 frames-per-second
	
	Grid theGrid(screen_width,
							 screen_height,
							 grid_width,
							 grid_height,
							 num_squares);
	
	theGrid.init(WHITE);
	
	unsigned frame_count = 0;
	float secs_per_clear = 0.25;
		
	// Main game loop
	while (!WindowShouldClose())    // Detect window close button or ESC key
    {
			if (IsKeyPressed(KEY_SPACE))
				paused = !paused; // toggle
			
			BeginDrawing();
			ClearBackground(RAYWHITE);
			
			//  ---------------------------------------------------------------  //
			//  Example of doing something every n secs.                         //
			//  ---------------------------------------------------------------  //

			/*
				if (frame_count % (fps*secs_per_clear) == 0) {
				ClearBackground(RAYWHITE);
				frame_count = 0;
				} 
				frame_count++;
			*/

			if (frame_count % static_cast<int>(fps*secs_per_clear) == 0) {
				int rand_row = GetRandomValue(0, num_squares-1);
				int rand_col = GetRandomValue(0, num_squares-1);
				theGrid.color(rand_row, rand_col, RED);
				frame_count = 0;
			}
			frame_count++;

			if (!paused) {
				theGrid.draw();
			}
			
			// Print FPS
			std::stringstream fps_msg;
			fps_msg << "FPS: " << GetFPS();
			
			DrawText(fps_msg.str().c_str(), 10, 700, 20, BLUE);
			
			if (paused)
				DrawText("Paused", 300, 350, 40, GREEN);
			
			EndDrawing();
    }
	
	CloseWindow();        // Close window and OpenGL context
	
	return 0;
}


