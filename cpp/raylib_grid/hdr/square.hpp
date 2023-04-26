#pragma once

#include <iostream>
#include "raylib.h"

class Square {

public:
	Square() = default;
	Square(int x, int y, int width, int height, Color color) :
		x_{x}, y_{y}, width_{width}, height_{height}, color_{color} {};
	~Square() = default;

	void draw() {
		if (this->is_valid()) {
			DrawRectangle(x_, y_, width_, height_, color_);
		}		
	}
	
	void on(const Color& c) {
		color_ = c;
	}
	
	void off() {
		color_ = WHITE;
		this->draw();
	}

	void set_color(Color c) {
		color_ = c;
	}
	
	bool is_valid() {
		return (x_ != -1 &&
						y_ != -1 &&
						width_  != -1 &&
						height_ != -1);
	}
	
	int x_      = -1;
	int y_      = -1;
	int width_  = -1;
	int height_ = -1;
	Color color_ = WHITE;	
};



