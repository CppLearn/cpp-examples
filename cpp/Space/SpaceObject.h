// -------------------------------------------------------------------------
//  File:    SpaceObject.h
//  Created: Fri Nov 09 21:08:47 2001
//  Comment: Generic SpaceObject class.  Base class of all space entities.
// -------------------------------------------------------------------------

#ifndef SPACEOBJECT_H

#define SPACEOBJECT_H

#include <iostream>
#include <string>
#include "SpaceCoords.h"

using namespace std;

//  ---------------------------------------------------------------  //
//  This class represents any space object. There will probably be   //
//  subclasses of this class. The space field will operate through   //
//  pointers to this base class. However it is not an abstract       //
//  class because there may be space junk or other simple objects    //
//  which are objects of this class.                                 //
//  ---------------------------------------------------------------  //

class CSpaceObject {

  // friend &ostream operator<<( ostream& ostr, SpaceObject& SO);

public:

  CSpaceObject();
  CSpaceObject(const string& Name);
  CSpaceObject(const CSpaceObject& right);
  ~CSpaceObject();
  
  virtual void print();
  virtual CSpaceCoords move(CSpaceCoords& Pos, double& Time);

//  ---------------------------------------------------------------  //
//  Get / set operations.                                            //
//  ---------------------------------------------------------------  //
  
  string get_name() const;
  void set_name(const string& Value);

  string get_symbolFile() const;
  void set_symbolFile(const string& Value);

  double get_condition() const;
  void set_condition(const double& Value);

  double get_weight() const;
  void set_weight(const double& Value);

  double get_velocity() const;
  void set_velocity(const double& Value);

  double get_heading() const;
  void set_heading(const double& Value);

  double get_acceleration() const;
  void set_acceleration(const double& Value);

//  ---------------------------------------------------------------  //
//  End get/set methods.                                             //
//  ---------------------------------------------------------------  //


private:

  double dist_covered(double& Time);

  string	   m_name;
  string	   m_symbolFile;

  
  double	   m_condition;
  double	   m_weight;
  double	   m_velocity;
  double	   m_heading;
  double	   m_acceleration;

};

#endif




