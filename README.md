# 9ML-toolkit

Code generation utility for neural network models implemented in the
NineML (http://www.nineml.net) language.

# Installation

In order to install the Chicken 9ML toolkit, you must first install
Chicken Scheme and then use the chicken-install program to download
and compile the 9ML toolkit.

## Prerequisites

You will need to obtain and install the Chicken Scheme
compiler and programming environment. The latest official release is
available from the following URL:

http://code.call-cc.org/releases/4.10.0/chicken-4.10.0.tar.gz

On Unix and Mac OS X there are no additional prerequisites, but on
Windows, you will need to first install either one of the MinGW or
Cygwin packages, because Chicken must be compiled with the gcc
compiler.

## Installation of Chicken Scheme

First, you will need to download the Chicken source archive from the
above location, then unpack it and change to the directory extracted
from the archive:

```
   tar zxf chicken-4.10.0.tar.gz
   cd chicken-4.10.0
```

Then, you need to compile and install:

On Linux:

```
make PLATFORM=linux PREFIX=$HOME/chicken install
```

On Mac OS X (64-bit):

```
make PLATFORM=macosx ARCH=x86-64 PREFIX=$HOME/chicken install
```

On Mac OS X (32-bit):

```
make PLATFORM=macosx PREFIX=$HOME/chicken install
```

On MinGW or Cygwin:

```
make PLATFORM=mingw PREFIX=C:\chicken install
make PLATFORM=cygwin PREFIX=C:\chicken install
```

## Installation of 9ML toolkit:

After Chicken Scheme has been successfully installed, the 9ML toolkit can be installed using the chicken-install program:

```
  ~/chicken/bin/chicken-install 9ML-toolkit
```

(On Windows, the tilde must be replaced with C:\ or whichever drive
you have installed Chicken on).


## Installing the MLton compiler

In order to compile and run network models, you will also need the MLton compiler, which is available here:

http://sourceforge.net/projects/mlton/files/mlton/

After you download, build and install MLton, please make sure that the
directory where it is installed is included in your PATH.

For example, if MLton resides in /usr/local/mlton, then add
/usr/local/mlton/bin to your PATH:

```
PATH=/usr/local/mlton/bin:$PATH

export PATH
```


# Usage: 

> 9ML-network operand1... [options...] 

Where operands are NineML user layer files.

The following options are recognized: 

* `--platform=PLATFORM`:   simulation platform (one of mlton, chicken)
* `-m, --method=PLATFORM`:  integration method (one of rkfe, rk3, rk4a, rk4b, rkoz, rkdp)
* `-d, --duration=VALUE`:  simulation duration in milliseconds
* `--timestep=VALUE`:  simulation timestep milliseconds
* `-s, --spikerecord=POPULATION`:  name of population for spike recording
* `--statesample=VALUE`:  sample size of neurons for state recording
* `--extsample=VALUE`:  sample size of neurons for external input recording
* `-v, --verbose`:  prints detailed information about the internal representation of the model during the code generation process

