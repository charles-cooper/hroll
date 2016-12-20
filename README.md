# hroll
Haskell implementations of rolling algorithms.

The basic data structure used is a FIFO queue with an accumulator parameter in the style of Okasaki. With that we can implement
- rolling max / min
- rolling sum
- rolling covariance
