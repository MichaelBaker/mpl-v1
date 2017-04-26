-- | A compiler can be though of generically as a pipeline of functions that transform a programs representation from one form to another.
--
-- I think ideally each phase in the pipeline would be able to fuse into a single pass over the input, while still being represented as a pipeline for conceptual simplicity. However, for now I'm representing it as a literal pipeline in order to make it easier for me to implement.
--
-- This module contains a set of functions for building these piplines in a consistent way.
module Mpl.Compiler where

