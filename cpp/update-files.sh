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
# C++ quick files
#
cp -rpv ~/git/cpp/quick/*.cc quick
git add quick/*.cc

#
# CL libs
#
cp -rpv ~/git/lisp/libs/dice.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/fizzle.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/grid.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/matrix.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/moth.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/pixel.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/slip.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/unix.clisp.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/unix.clozure.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/unix.lsp ../cl/libs
cp -rpv ~/git/lisp/libs/unix.sbcl.lsp ../cl/libs

git add ../cl/libs/*.lsp

#
# PlantUML
#
cp -pv ~/git/micros/plant_uml/*.pu ../plant/

git commit -a -m "updating files..."



