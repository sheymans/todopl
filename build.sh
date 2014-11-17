#!/bin/bash

PROLOG_SRC_LOCATION=./src/todopl/prolog
EXCHANGE_SRC_LOCATION=./src/foreign/ms_exchange
VERSION=1.1.2

clear

echo "Starting a JAVA build..."
echo

echo "Creating Prolog server..."
swipl --goal="todopl(5000)" --stand_alone=true --foreign=save -o todopl_server -c $PROLOG_SRC_LOCATION/scheduling_server.pl
echo "==> Prolog server created."
echo

echo "Compiling MS Exchange Java code..."
THISDIR=$(pwd)
cd $EXCHANGE_SRC_LOCATION
mvn clean package install
cd $THISDIR
echo "==> Exchange jar installed in your local maven repository."
echo


echo "Building todopl JAR..."
lein uberjar
echo "...done."
echo

