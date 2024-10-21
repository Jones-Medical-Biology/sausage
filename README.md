# Sausage

## Info

This is a general-purpose bioinformatics library with the goal of having excellent

 1. documentation,
 2. reproducibility, and
 3. extensibility

with less debuging. The choice of Haskell is to make it possible for
researchers to expertly define workflows and pipelines without
requiring expertise in software engineering.

### Goals
#### Add Tools
 - [ ] UMAP
 
#### Modularize Pipelines
 - [ ] Tools should be compression agnostic such that the tool
       receives a feed of data in an uncompressed format and outputs
       data in an uncompressed format such that any arbitrary
       decompressor and compressor can be composed with the tool.

## Notes
### 2023-11-04
I think that maybe [this article](
https://towardsdatascience.com/how-to-program-umap-from-scratch-e6eff67f55fe#:~:text=As%20a%20test%20data%20set%2C,dimensions%20of%20the%20data%20set
) might be helpful. It suggests using Cancer-associated fibroblasts
(file located [here](
https://www.nature.com/articles/s41467-018-07582-3 ).

### 2023-11-12
Started constructing types and began to test a parser implementation
using Tsoding's JSON parser YouTube video as reference.
The corresponding repo: https://github.com/tsoding/haskell-json

### 2024-01-31
I am recruiting some help to work on this project, and I need to state
some clear goals and objectives.

### 2024-03-07
We will want to increase the priority of the AI component in light of the
new scholarship/grant opportunity.

### 2024-04-25
What would be really helpful would be a dimensionality-reduction tool
to show distances between sequences and molecules, and I think it
would make a lot of sense to do point-by-point similarity where the
similarity at a point falls off exponentially with distance from the
point, which then is computed to the point score. The idea is that if
two sequences are separated by a repeat or some slippage or
duplication within a region for whatever reason that this will not
completely change its similarity score. It seems like the comparison
must be done itteratively with mers all the way up to the length of
the molecule. Then, the UMAP process should be done because ultimately
two sequences can each be similar to a third sequence while having no
similarity between themselves.

## FASTA
Starting a new parser specifically for ATCG documents. The goal is to 
use minimal datatypes for the base so  that it can be used for streamlined
ML applications without much extra work especially that on purpose-built
hardware.

# Type-level Metaprogramming
New near-term goal is a reflection of the grant recently written.
