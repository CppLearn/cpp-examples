#!/usr/bin/sh

echo "updating quickcpp.cpp..."
cp -pv ~/git/cpp/QuickCPP/quickcpp.cpp .
echo "building quickcpp..."
make quickcpp
git add quickcpp.cpp
cp -rpv ~/git/cpp/quick/*.cc quick
git add quick/*.cc
cp -rpv ~/git/micros/graphviz/*.dot ../dot
git add ../dot/*.dot
cp -rpv ~/git/lisp/libs/*.lsp ../lisp/libs
git add ../lisp/libs/*.lsp
git commit -a -m "updating files..."


