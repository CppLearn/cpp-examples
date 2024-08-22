
#include "SpaceContainer.h"

int CSpaceContainer::
add_object( CSpaceObject* SpaceObj )
{
  CContainerObject container_obj;
  
  container_obj.m_id = ++m_current_id;
  container_obj.p_SpaceObj = SpaceObj;

  m_container.push_back(container_obj);
}

void CSpaceContainer::
del_object( int SpaceObjID )
{
;
}

void CSpaceContainer::
print_objects()
{

  cout << "\n Dumping contents of container.." << endl;

  for( m_iterator = m_container.begin(); 
       m_iterator != m_container.end(); m_iterator++ ) {

    cout << "\n SpaceObject ID: " << m_iterator->m_id;
    m_iterator->p_SpaceObj->print();

  }


}


