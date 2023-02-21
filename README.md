
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FACT

<!-- badges: start -->
<!-- badges: end -->

Understanding how assignments of instances to clusters can be attributed
to the features can be vital in many applications. We present FACT
(Feature Attributions for ClusTering), an algorithm-agnostic framework
that preserves the integrity of the data and does not introduce
additional models.

## Features

- SMART (Scoring Metric AfteR permuTation) measures changes in cluster
  assignments by custom scoring functions after permuting selected
  features
- IDEA (IsolateD Effect on Assignment) indicates local and global
  changes in cluster assignments after making uniform changes to
  selected features.

## Installation

You can install the development version of FACT like so:

``` r
# Development version
remotes::install_github("henrifnk/FACT")
```

## Quickstart

We want to divide American states by their standardized crime rates in 3
clusters.

``` r
library(FACT)
library(mlr3cluster)
#> Lade nötiges Paket: mlr3
attributes_scale = attributes(scale(USArrests))
```

|            | Murder | Assault | UrbanPop |  Rape |
|:-----------|-------:|--------:|---------:|------:|
| Alabama    |   1.24 |    0.78 |    -0.52 |  0.00 |
| Alaska     |   0.51 |    1.11 |    -1.21 |  2.48 |
| Arizona    |   0.07 |    1.48 |     1.00 |  1.04 |
| Arkansas   |   0.23 |    0.23 |    -1.07 | -0.18 |
| California |   0.28 |    1.26 |     1.76 |  2.07 |
| Colorado   |   0.03 |    0.40 |     0.86 |  1.86 |

USArrests Data Set

Therefore, we use a c-means algorithm from `mlr3cluster`.

``` r
tsk_usa = TaskClust$new(id = "usarest", backend = data.frame(scale(USArrests)))
c_lrn = lrn("clust.cmeans", centers = 3, predict_type = "prob")
c_lrn$train(tsk_usa)
```

Then, we create a `ClustPredictor` that wraps all the information needed
for our methods.

``` r
predictor = ClustPredictor$new(c_lrn, data = tsk_usa$data(), y = c_lrn$model$membership)
```

How does `Assault` effect the clustering outcome created by the
`c-means`?

``` r
idea_assault = IDEA$new(predictor, "Assault", grid.size = 50)
idea_assault$plot_globals(0.5)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="65%" style="display: block; margin: auto;" />

# Citation

If you use FACT in a scientific publication, please
[cite](https://arxiv.org/abs/2209.10578) it as:

    Scholbeck, C. A., Funk, H., & Casalicchio, G. (2022). Algorithm-Agnostic Interpretations for Clustering. arXiv preprint arXiv:2209.10578.

BibTeX:

``` tex
@article{FACT_22,
  title={Algorithm-Agnostic Interpretations for Clustering},
  author={Scholbeck, Christian A and Funk, Henri and Casalicchio, Giuseppe},
  journal={arXiv preprint arXiv:2209.10578},
  year={2022}
}
```
