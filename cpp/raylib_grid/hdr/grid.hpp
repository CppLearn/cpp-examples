#pragma once

#include <cmath>
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>
#include "raylib.h"
#include "square.hpp"

class Grid {
	
public:
	
	Grid() = default;
	Grid(int s_width, int s_height, int g_width, int g_height, int num_squares) :
		screen_width_{s_width},
		screen_height_{s_height},
		grid_width_{g_width},
		grid_height_{g_height},
		num_squares_{num_squares}
	{
		box_width_  = (grid_width_  / num_squares_);
		box_height_ = (grid_height_ / num_squares_);		
	}
	
	// Grid& Grid(const Grid& other) = default;
	Grid& operator=(const Grid& other) = default;
	~Grid() = default;
	
	void draw() {
		for(int i = 0; i < num_squares_; i++) {
			for (int j = 0; j < num_squares_; j++) {
				grid_[i][j].draw();
			}
		}
	};

	void color(int row, int col, Color color) {
		grid_[row][col].on(color);
	}
	
	void init(const Color& c) {

		int left_over_width = screen_width_ - grid_width_;
		int left_over_height = screen_height_ - grid_height_;
		
		spacing_horiz_ = floor(left_over_width / num_squares_);
		spacing_vert_ = floor(left_over_height / num_squares_);
		
		int x, y; // coordinates of current square.
		
		for(int i = 0, y = spacing_vert_; i < num_squares_; i++) {
			
			std::vector<Square> new_row;
			
			for(int j = 0, x = spacing_horiz_; j < num_squares_; j++) {				
				new_row.push_back(Square(x, y, box_width_, box_height_, c));				
				x += box_width_ + spacing_horiz_;				
			} // end cols;
			
			grid_.push_back(new_row);
			y += box_height_ + spacing_vert_; // next y
				
		} // end rows;
		
		std::cout << "\n =================================";
		std::cout << "\n boxes have been created!!";
		std::cout << "\n size of grid: " << grid_.size();
		
	};
	
	//  ---------------------------------------------------------------  //
	//  Instead of drawing a grid, draw squares next to each other.      //
	//  ---------------------------------------------------------------  //
	
private:

	int grid_width_  = -1;
	int grid_height_ = -1;
	
	int screen_width_  = -1;
	int screen_height_ = -1;
	
	int num_squares_  = -1;
	int box_width_    = -1;
	int box_height_   = -1;
	
	int spacing_horiz_ = 0;
	int spacing_vert_  = 0;
	
	Color color_ = BLUE;
		
	std::vector< std::vector<Square> > grid_;
	
};

