# code_and_writing_samples
Code and writing samples for Judah Axelrod

## Stat_506_HW7.pdf

This is a recent assignment in my Advanced Regression Analysis class with Dr. Ian Laga. Since the format is a short paper, the context of the writing is within the file.

## get_adjacency_matrices.R

The goal with this file was to create networks containing all senators who participated in each 2-year period between elections from the 101st congress to the 111th. We populated two matrices containing two different bits of information called _votes_ and _y_. The _(i,j)th_ position in _votes_ is a number that represents the number of times senator _i_ and senator _j_ both voted "yea" or both voted "nay" on a bill.  The _(i,j)th_ position in _y_ is a number that represents the number of votes in which both senator _i_ and senator _j_ both participated. The purpose of _y_ is that sometimes a senator is appointed to a cabinet or ambassador position or passes away, and a special election is held. In these cases, not all senators voted the same number of times. This is important because we are modeling each cell in _votes_ to have a binomial distribution. One of the parameters of the binomial is the number of trials which is information contained in _y_.
