#!/usr/bin/sh

echo "updating quickcpp.cpp..."
cp -pv ~/git/cpp/QuickCPP/quickcpp.cpp .
echo "building quickcpp..."
make quickcpp
git add quickcpp.cpp
cp -rpv ~/git/cpp/quick/*.cc quick
git add quick/*.cc
git commit -a -m "updating cpp files..."

