TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    bst-tests.cpp \
    bst.cpp

HEADERS += \
    bst.h

LIBS += -lboost_unit_test_framework
