#ifndef SPACETRACKER_H
#define SPACETRACKER_H

#include <iostream>

class CSpaceTracker {

public:

	CSpaceTracker(const string& FuncName) 
	{
		m_functionName = FuncName;
		cout << "\n [Starting] " << m_functionName << endl;
	}
			
	~CSpaceTracker()
	{
		cout << "\n\n [Exiting] " << m_functionName << endl;
	}

private:
	string m_functionName;

};


#endif
