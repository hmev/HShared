## Introduction

Intent to give a simple prototype for operational work sharing.

## Version 0.1

First Edition :
    1 Enable element creation and modification
    2 Enable element episode as pure Int
    3 Do not consider multi-user and multi-files
    4 Give a simple test environment

## Version 0.2

    1 Enable undo/redo
      - use a mutable queue to represent the undo mechanism.
    2 Enable element deletion
      - Just delete it from element table map.
      - If still using transaction mementos, save it as a memento item.

## Version 0.3
    1 Revise some of data structures to half-purity
    2 Enable operation read / save from file
    3 Enable undo/redo operation

## Version 0.4 (Planning)
    1 Enable basic exception throwing and handling
    2 Add exception handling mechanism