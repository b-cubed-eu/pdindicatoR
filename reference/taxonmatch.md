# Taxon matching

This function matches the tip labels of a phylogenetic tree (Taxon names
or OTT id's) with corresponding GBIF id's.

## Usage

``` r
taxonmatch(tree)
```

## Arguments

- tree:

  An object of class `'phylo'`, a phylogenetic tree in Newick format
  that was parsed by `ape::read_tree()`

## Value

A dataframe with as variables (among others) the search strings, matched
OTT names and id's, gbif id's and original tiplabels

## Examples

``` r
if (FALSE) ex_data <- retrieve_example_data()
# This can take a while!
mtable <- taxonmatch(ex_data$tree) # \dontrun{}
#> Error: object 'ex_data' not found
```
