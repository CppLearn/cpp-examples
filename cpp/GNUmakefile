RM_CMD := -rm -f

CC := g++
CC_FLAGS := -g -std=c++11
LIBS := -lm
LINK_FLAGS := 
INC_FLAGS := -I /usr/include

#----------------------------------------------------------------------------- 
# Pattern Rules
#----------------------------------------------------------------------------- 

% : %.cpp
	@echo ""	
	@echo "Compiling $< ..."
	$(CC) $(CC_FLAGS) $(DEFINES) $(INC_FLAGS) $< -o $@
	#compiled
	@echo ""
