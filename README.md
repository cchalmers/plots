# plots

[![Haddock](https://rawgit.com/cchalmers/plots/gh-pages/haddock.svg)](https://cchalmers.github.io/plots/)
[![Travis](https://api.travis-ci.org/cchalmers/plots.svg?branch=master)](https://travis-ci.org/cchalmers/plots)
[![Hackage](https://img.shields.io/hackage/v/plots.svg)](https://hackage.haskell.org/package/plots)


`plots` is a plotting library based on [diagrams](http://projects.haskell.org/diagrams).

Some sample plots:

#### Scatter plot
![scatter-plot](https://rawgit.com/cchalmers/plots/master/diagrams/src_Plots_Types_Scatter_scatterExample'.svg)

#### Bar plot
![bar-plot](https://rawgit.com/cchalmers/plots/master/diagrams/src_Plots_Types_Bar_barExample'.svg)

#### Heat map
![heat-map](https://rawgit.com/cchalmers/plots/master/diagrams/src_Plots_Types_HeatMap_heatMapIndexedExample'.svg)

There are many more plots to be added. There are also plans to support
3D plots. Issues and pull requests welcome.

## Examples

There are several example plots in the `examples/` directory. To build
the examples, first ensure that you have `stack` installed, and that you
have a `stack.yaml` file in this directory that contains the lines:

```yaml
packages:
- '.'
- 'test'
```

(If you run the command `stack init`, stack will automatically generate
the `stack.yaml` file with the appropriate packages.)

You can then build the examples using ```sh stack build stack exec
RunExamples ``` The `RunExamples` script will generate `png` files in
the `examples_output` directory.

**Note:** The `RunExamples` script re-builds an example if the output
file (`examples_output/X.png`) does not exist, or if the source file
(`examples/X.hs`) has been modified since the last time the script was
run.
