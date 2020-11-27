TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    hashtable-tests.cpp

HEADERS += \
    hashtable.h

LIBS += -lboost_unit_test_framework
