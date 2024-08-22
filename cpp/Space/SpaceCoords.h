//  -------------------------------------------------------------------------
//   File:    SpaceCoords.h
//   Created: Sat Nov 10 20:33:00 2001
//   Comment: This class manages coordinates in the space coordinate system.
//            The graphical implementation may use a scaled coordinate system.
//            All SpaceObjects will operate in the space coord sys.
//  -------------------------------------------------------------------------

#ifndef SPACECOORDS_H
#define SPACECOORDS_H

#include <iostream>

using std::cout;

struct CSpaceCoords {

  CSpaceCoords() = default;
  CSpaceCoords(double X, double Y) : x_{X}, y_{Y} {}; 
  
  double x_ = 0.0;  // x space coordinate.
  double y_ = 0.0;  // y space coordinate.

};

inline std::ostream& operator<< (std::ostream& out, const CSpaceCoords& coords) {
	return out << "\n x = " << coords.x_ << "\n y = " << coords.y_;
}

#endif







