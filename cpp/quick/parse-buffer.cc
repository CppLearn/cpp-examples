
__main__

std::string buffer = "'{ CIRCLE 724 452 25 0 255 0 0 }'\n'{ CIRCLE 768 275 25 0 255 0 0 }'";



	/*
                      "{ CIRCLE 768 275 25 0 255 0 0 }"
											{ CIRCLE 506 427 25 0 0 0 0 }\n
											{ CIRCLE 678 231 25 0 0 0 0 }\n
											{ CIRCLE 266 53 25 0 255 0 255 }\n
											{ CIRCLE 128 496 25 0 0 255 255 }\n
											{ CIRCLE 402 1 25 0 255 0 255 }\n
											{ CIRCLE 616 8 25 0 0 0 255 }\n
											{ CIRCLE 766 181 25 0 0 255 0 }\n
											{ CIRCLE 4 183 25 0 255 0 0 }\n
											{ CIRCLE 600 302 25 0 0 255 0 }\n
											{ CIRCLE 80 135 25 0 255 255 255 }\n
											{ CIRCLE 443 433 25 0 255 0 0 }\n
											{ CIRCLE 362 421 25 0 0 255 255 }\n
											{ CIRCLE 739 302 25 0 0 0 0 }\n
											{ CIRCLE 396 330 25 0 255 0 255 }\n
											{ CIRCLE 442 289 25 0 0 0 0 }\n
											{ CIRCLE 757 488 25 0 255 0 0 }\n
											{ CIRCLE 79 16 25 0 0 0 0 }\n
											{ CIRCLE 569 58 25 0 255 255 255 }"
	*/


std::istringstream iss(buffer);
std::string line;
std::vector<std::string> lines;

while (std::getline(iss, line)) {
	std::cout << line << std::endl;
 }


