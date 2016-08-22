## Qanalyst
An R package for building and visualizing quality control charts. Depends also on a package not on CRAN at the moment but available on my Github: [chartconstants](https://github.com/mick001/chartconstants).

Please note that the package is currently under development so it probably is buggy and the documentation is still in progress.

Requires:
- dplyr (>= 0.3.0.2)
- lazyeval
- ggplot2

So far the implemented functions include:

- Functions for generating control charts (SE and NSE version).
- Functions for setting user defined parameters on a chart.
- Function for simple static plotting.

Future features to be implemented:

- Integration with Shiny
- Interactive plotting (possibly with ggvis)

## Implemented charts
Currently the following charts are available:

- Charts for variables: xbar-s, xbar-r, s, r.
- Charts for attributes: c, p, np, I, mr, u.

## Examples
Each function is documented with an example, check each function documentation. I'm going to write a tutorial on the package when it is finished.

## License
See the LICENSE file for license rights and limitations (GPL 3.0).
