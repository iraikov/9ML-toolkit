# Brunel network model

This folder contains two implementations of the network model described in:

   Brunel, N (2000) Dynamics of Sparsely Connected Networks of Excitatory and Inhibitory Spiking Neurons.
   Journal of Computational Neuroscience 8(3):183--208. doi:10.1023/A:1008925309027.

The implementation in brunel_network_alpha.scm uses current-based
synapses with an alpha-function dynamics, and the implementation in
brunel_network_delta.scm uses delta synapses as in the original paper.



# Running simulations: 

The following command will generate and compile model code from the
NineML description of one of the model variants:

> 9ML-network -m crk3 brunel_network_alpha_... 

The `9ML-network` program will create an executable named `Sim_{model name}` which accepts the following arguments:

* `-d, --duration=VALUE`:  simulation duration in milliseconds
* `--timestep=VALUE`:  simulation timestep milliseconds
* `-s, --spikerecord=POPULATION`:  name of population for spike recording
* `--statesample=VALUE`:  sample size of neurons for state recording
* `--extsample=VALUE`:  sample size of neurons for external input recording
* `-v, --verbose`:  prints detailed information about the internal representation of the model during the code generation process

For example, the following command will run a simulation for 1200.0
milliseconds with timestep 0.01 ms and will record spike times from
the "All neurons" population set:

> Sim_brunel_network_alpha_AI -d 1200.0 --timestep=0.01 -s "All neurons"


# Generating model XML:

The scripts brunel_network_alpha.scm and brunel_network_delta.scm
generate NineML XML descriptions of the two Brunel model
implementations with different parameters. These scripts are
implemented in Chicken Scheme and can be run as follows:

> csi -s brunel_network_alpha.scm

where csi is the executable of the Chicken Scheme interpreter.

When run, these scripts will generate files of the form:

```
brunel_network_alpha_AI.xml
brunel_network_alpha_SI.xml
brunel_network_alpha_SR.xml
brunel_network_alpha_AR.xml
```

which are parameterizations of the model for each of the four regimes discussed in the paper.

In addition, the script will generate files of the form:

```
brunel_network_alpha_g1.0_eta1.0.xml
...
brunel_network_alpha_g8.0_eta8.0.xml
```

These are parameterizations of the model where the g (inhibitory
synaptic weight) and eta (external stimulus rate) are varied in the range 1..8.
