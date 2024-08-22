// -------------------------------------------------------------------------
//  File:    SpaceObject.cpp
//  Created: Fri Nov 09 21:33:21 2001
//  Comment: Implementation of SpaceObject class.
// -------------------------------------------------------------------------

#include <cmath>
#include "SpaceObject.h"

//  ---------------------------------------------------------------  //
//  Default Constructor.                                             //
//  ---------------------------------------------------------------  //

CSpaceObject::CSpaceObject() 
{

  cout << "\n" << "Creating space object ..." << endl;
    
  set_name("");

  set_condition(100.0);
  set_weight(0.0);
  set_velocity(0.0);
  set_heading(0.0);
  set_acceleration(0.0);

}

//  ---------------------------------------------------------------  //
//  Probably most commonly used constructor.                         //
//  ---------------------------------------------------------------  //

CSpaceObject::CSpaceObject(const string& Name) 
{
  set_name(Name);

  set_condition(100.0);
  set_weight(0.0);
  set_velocity(0.0);
  set_heading(0.0);
  set_acceleration(0.0);
}

CSpaceObject::~CSpaceObject()
{
  cout << "\n...Destroying [" << 
    "space object" << "]-> " << get_name() << endl;
}

CSpaceObject::CSpaceObject(const CSpaceObject& right)
{
  set_name(right.get_name());
  set_condition(right.get_condition());
  set_weight(right.get_weight());
  set_velocity(right.get_velocity());
  set_heading(right.get_heading());
  set_acceleration(right.get_acceleration());
  
}

//  ---------------------------------------------------------------  //
//  Get / set operations.                                            //
//  ---------------------------------------------------------------  //
 
string CSpaceObject::get_name() const
{
  return m_name;
}

void CSpaceObject::set_name(const string& Value)
{
  m_name = Value;
}

double CSpaceObject::get_condition() const
{
  return m_condition;
}

void CSpaceObject::set_condition(const double& Value)
{
  m_condition = Value;
}

double CSpaceObject::get_weight() const
{
  return m_weight;
}

void CSpaceObject::set_weight(const double& Value)
{
  m_weight = Value;
}

double CSpaceObject::get_velocity() const
{
  return m_velocity;
}

void CSpaceObject::set_velocity(const double& Value)
{
  m_velocity = Value;
}
  
double CSpaceObject::get_heading() const
{
  return m_heading;
}

void CSpaceObject::set_heading(const double& Value)
{
  m_heading = Value;
}

double CSpaceObject::get_acceleration() const
{
  return m_acceleration;
}

void CSpaceObject::set_acceleration(const double& Value)
{
  m_acceleration = Value;
}

//  ---------------------------------------------------------------  //
//  End get/set methods.                                             //
//  ---------------------------------------------------------------  //

void CSpaceObject::print()
{
  cout << "\n SpaceObject: " << get_name() << endl;

  cout << endl;
  cout << "\t condition = " << get_condition() << endl;
  cout << "\t weight = " << get_weight() << endl;
  cout << "\t velocity = " << get_velocity() << endl;
  cout << "\t heading = " << get_heading() << endl;
  cout << "\t acceleration = " << get_acceleration() << endl;

}

double CSpaceObject::dist_covered(double& Time)
{

//  ---------------------------------------------------------------  //
//  Compute distance covered during time elapsed.                    //
//  This only computes the magnitude of the distance covered and     //
//  does not take into consideration the actual distance covered     //
//  if the heading of the vector has changed.                        //
//  ---------------------------------------------------------------  //

  double new_dist = 
    (get_velocity() * Time) +		// add current velocity dist.    
    (0.5 * get_acceleration()		// add acceleration dist.
     * Time * Time);
  
  return new_dist;
}

//  ---------------------------------------------------------------  //
//  Compute new position based on current position and time.         //
//  Current position is not part of object like velocity and         //
//  acceleration. i.e. the SpaceObject knows about how fast it is    //
//  moving but it doesnt know where it is. The class representing   //
//  Space will know where the objects are. The SpaceObject has a     //
//  move method because it knows how far it can travel in a given    //
//  time. The Space class will use the SpaceObjects method to        //
//  figure out how far it moves.                                     //
//  ---------------------------------------------------------------  //

CSpaceCoords CSpaceObject::move(CSpaceCoords& Pos, double& Time)
{
  CSpaceCoords newPos;
  
  double add_x = cos(get_heading()) * dist_covered(Time);
					// adjust x component for heading.
  double add_y = sin(get_heading()) * dist_covered(Time);
					// adjust y component for heading.
  newPos.x_ = (Pos.x_ + add_x);
  newPos.y_ = (Pos.y_ + add_y);
  
  return newPos;
}

