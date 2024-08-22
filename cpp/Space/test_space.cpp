#include <iostream>
#include "SpaceObject.h"
#include "SpaceCoords.h"
#include "SpaceContainer.h"
#include "SpaceSys.h"

using std::cout;

int main()
{
  CSpaceTracker ft("test_space main");

	/*
  CSpaceObject barge;
  barge.set_name("KF7-B001");
  barge.set_weight(3473);

	std::cout << barge;
	
  CSpaceObject space_junk;
  space_junk.set_name("piece of debris");
  space_junk.set_weight(234);

	std::cout << space_junk;
	*/
	
  cout << "\nTesting Space coords: " << endl;
  
  CSpaceCoords coords(400.0, 640.0);
	std::cout << "\n " << coords << "\n";
  
  CSpaceCoords c2(100.0, 120.0);
	std::cout << "\n " << coords << "\n";

	/*
		CSpaceContainer theContainer;
		
		theContainer.add_object( &barge );
		theContainer.add_object( &space_junk );
		theContainer.print_objects();
	*/

	
		
}


