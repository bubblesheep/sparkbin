## Optimal Binning For Spark Using Sparklyr

### Proposed Steps

1. A prepared data with target (0/1) and various candidate features that do not need to be pre-processed
2. **Initial Binning**: We perform initial binning into a number of fine classes by equal count in sparklyr. We can define classes for the output for better structure definition.
3. **Optimal Binning**: The data from initial binning are small enough so we can do optimal binning in local R using Tree based methods. The output of optimal binning should be in same class and same structure as initial binning
4. **Binning Transformation**: We use the optimal binning objects to transform the spark data frame using sparklyr and ready to do modeling
5. The transformed data can be fitted using logistic regression using mlib in Spark


### Binning Class Data Structure

The binning class data structure is essential in the whole process.

##### Numeric Binning (intervalbin class)
The intervalbin class contains the following fields
- **cuts**: the cutting points from -Inf to Inf, example `c(-Inf, 1.5, 10, Inf)`
- **good**: the vector of counts for good (0) of the buckets created by cuts, example `c(200, 300, 500)`
- **bad**: the vector of counts for bad (1) of the buckets created by cuts, example `c(100, 300, 900)`
- **\_Missing\_**: the good/bad counts for missing values, example `c(good = 50, bad = 50)`

An example of creating binning object for numeric is
```
obj <- list(cuts = c(-Inf, 1.5, 10, Inf),
             good = c(200, 300, 500),
             bad = c(100, 300, 900),
             _Missing_ = c(good = 50, bad = 50))

class(obj) <- "intervalbin"
```

#### Character Binning (nominalbin class)
The nominalbin class contains the following fields

- **xlevels**: The original levels of the character vector, special handling should be taken. 1. missing values are set to level \_Missing\_. 2. The levels having frequency below **minp** are set to level \_Other\_. Example `c("A", "B", "_Missing_", "_Other_")`
- **ylevels**: The mapped new level, which should be coarser than xlevels. For the initial binnig step, xlevels and ylevels should be same. Example: `c("A", "G", "G", "G")`
- **good**: the count of good for each level in xlevels
- **bad**: the count of bad for each level in xlevels
- **minp**: the threshold to set a level to Other

An example of creating binning object for character is

```
obj <- list(xlevels = c("A", "B", "_Missing_", "_Other_"),
             ylevels = c("A", "B", "_Missing_", "_Other_"),
             good = c(200, 200, 200, 200),
             bad = c(100, 200, 300, 400),
             minp = 0.01)
```
