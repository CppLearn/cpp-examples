#!/usr/bin/sh

#
# Quick Cpp
#
echo "updating quickcpp.cpp..."
cp -pv ~/git/cpp/QuickCPP/quickcpp.cpp .
echo "building quickcpp..."
make quickcpp
git add quickcpp.cpp

#
# Timer
#
echo "updating Timer..."
cp -pv ~/git/cpp/NanoTimer/nanotimer.hpp .
cp -pv ~/git/cpp/NanoTimer/nanotimer.cpp .
cp -pv ~/git/cpp/NanoTimer/nano_main.cpp .
cp -pv ~/git/cpp/NanoTimer/makefile .

#
# C++ quick files
#
cp -rpv ~/git/cpp/quick/*.cc quick
git add quick/*.cc

#
# DOT graph files
#
cp -rpv ~/git/micros/graphviz/*.dot ../dot
git add ../dot/*.dot

#
# CL libs
#
cp -rpv ~/git/lisp/libs/* ../cl/libs
git add ../cl/libs/*


git commit -a -m "updating files..."



