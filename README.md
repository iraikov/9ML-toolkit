# 9ML-toolkit

Code generation utility for neural network models implemented in the
NineML (http://www.nineml.net) language.

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
