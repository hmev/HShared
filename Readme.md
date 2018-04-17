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
      - Just delete it from element tabel map.
      - If still using transaction mementos, save it as a memento item.