#ifndef SPACECONTAINER_H
#define SPACECONTAINER_H

#include <iostream>
#include <vector>
#include "SpaceObject.h"

class CSpaceContainer {

public:

  // Default constructor.
  
  CSpaceContainer() { 
    m_container.reserve(100); 
    m_num_objects = 0;
    m_current_id = 0;
  }
    
  // Destructor.

  ~CSpaceContainer() { 
    
    cout << "\ndestroying SpaceContainer object..." << endl;
    
    m_container.resize(0);
    m_num_objects = 0;

  } // end destructor

  int add_object( CSpaceObject* SpaceObj );
  void del_object( int SpaceObjID );
  int num_objects() const { return m_num_objects; };
  void print_objects();

private:
  
  class CContainerObject{
    
  public:
    
    CContainerObject() : m_id(0), p_SpaceObj(0) {}
    ~CContainerObject() { 
      m_id = 0;
      p_SpaceObj = 0;
    }
    
    int m_id;
    CSpaceObject* p_SpaceObj;
    
  };

  // Data members

  int m_num_objects;
  int m_current_id;
  vector<CContainerObject> m_container; 
  vector<CContainerObject>::const_iterator m_iterator;

} ;



#endif




