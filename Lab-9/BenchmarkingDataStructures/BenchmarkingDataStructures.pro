TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    ../../BinaryTrees/bst.cpp \
    ../09-timing_tests/keyitemgenerator.cpp \
    ../09-timing_tests/timing_tests.cpp


HEADERS += \
    ../../BinaryTrees/bst.h \
    ../09-timing_tests/keyitemgenerator.h
