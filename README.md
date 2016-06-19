# plots

[![Haddock](https://rawgit.com/cchalmers/dense/gh-pages/haddock.svg)](https://cchalmers.github.io/plots/)

`plots` is a plotting library based on [Diagrams](http://projects.haskell.org/diagrams/) supporting 2D and 3D plots. The goal is make good looking plots easily with lots of customisability.

Currently in development stage so it doesn't work well yet. Feel free to take a look and make suggestions.

## Examples

There are several example plots in the `examples/` directory. To build the examples, first ensure that you have `stack` installed, and that you have a `stack.yaml` file in this directory that contains the lines:

```yaml
packages:
- '.'
- 'test'
```

(If you run the command `stack init`, stack will automatically generate the `stack.yaml` file with the appropriate packages.)

You can then build the examples using
```sh
stack build
stack exec RunExamples
```
The `RunExamples` script will generate `png` files in the `examples_output` directory.

**Note:** The `RunExamples` script re-builds an example if the output file (`examples_output/X.png`) does not exist, or if the source file (`examples/X.hs`) has been modified since the last time the script was run.
