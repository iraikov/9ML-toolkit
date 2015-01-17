#!/bin/sh

STAGING_DIR=$PWD/9ML-toolkit.d

mkdir -p $STAGING_DIR
mkdir -p $STAGING_DIR/bin

${CHICKEN_HOME}/bin/chicken-install -init $STAGING_DIR

rm $STAGING_DIR/setup-download.* $STAGING_DIR/tcp.* $STAGING_DIR/srfi-18.*

$CHICKEN_HOME/bin/chicken-install -deploy -prefix $STAGING_DIR  \
  datatype typeclass regex make bind vector-lib iset rb-tree easyffi flsim format-graph getopt-long input-parse lalr matchable mathh miniML static-modules \
  numbers object-graph random-mtzig dyn-vector signal-diagram silex sxml-transforms ssax sxpath defstruct uri-generic utf8 ersatz

$CHICKEN_HOME/bin/chicken-install -deploy -prefix $STAGING_DIR 9ML-toolkit

mv $STAGING_DIR/bin/9ML-network/9ML-network $STAGING_DIR
mv $STAGING_DIR/bin/9ML-ivp/9ML-ivp $STAGING_DIR
rm -rf $STAGING_DIR/bin


