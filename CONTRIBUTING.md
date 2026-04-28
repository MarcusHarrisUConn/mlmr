# Contributing to mlmr

Thank you for helping test `mlmr`.

The project is currently in an early public-beta stage. Feedback is especially
useful when it is tied to a concrete model, dataset structure, or reporting
workflow.

## Useful Feedback

Please open a GitHub issue if you notice:

- a model that should fit but fails;
- confusing model-building language;
- incorrect or unclear equations;
- APA table formatting problems;
- uploaded data that the app handles poorly;
- diagnostics that are missing or hard to interpret;
- places where the generated R code does not reproduce the app output.

## Good Issue Reports

When possible, include:

- the app tab or workflow step where the problem occurred;
- the model structure you were trying to fit;
- whether you used the example data or uploaded data;
- screenshots of confusing output;
- the generated R code if the problem involves reproducibility;
- any warning or error text.

## Development Notes

Before submitting changes, run:

```r
R CMD build .
R CMD check --no-manual --no-build-vignettes mlmr_0.1.0.tar.gz
```

The test suite currently includes backend smoke tests for formula generation,
centering, model fitting, equations, Tau labels, APA tables, and export helpers.
