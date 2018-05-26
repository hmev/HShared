## Introduction

Intent to give a simple prototype for feasible element-level shared system.
This's a learning program, in order to investigate both the implementation feasibilty of a Haskell program, and key points of buiding a partially distributed shared system.

## Features 

Basic Framework :
    - serialization 
      - element serialization
      - operation serialization
    - operation mementos (i.e., undo/redo controller)

Collaborate : 
    - clock

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
    2 Index elements by element id
      - As comparison, some systems index element using element name the defect of them are high-possibility of rename. Users may need to keep attention on defining element names.
        Still, some occasions are favor to this kind. Such as connect element with its family type.
      - Still need to enable indexing from some other methods. 
        For example : Agent openGL.
    3 Add clock sequece for operations and records
      - Use UUID to identify across documents. 
        Do not suppose those UUID are created sequentially, as there are several methods to create a UUID. Instead, control those by a mannually maintained list.
      - Set up new episode for when saving.
    4 Enable episode mananger & element id manager
      - Let episode creation and element id assign are done locally, i.e., by self document, and use universal unique id for consistence beyond document.
        It's a partially distributed design, see wiki page if wants to know more :
        https://en.wikipedia.org/wiki/Universally_unique_identifier

## Potential Problem
    1 Not sure if the none-return-value design of some data structrues could work well with haskell's delayed parsec and concurrent mechanism.
    2 Protection of operations on mutable data structures

    !!! 
    It's really hard enough to write a feasible system using Haskell, due to its delay-prasec feature. As this feature exsits, all values shall be passed explicitly to avoid potential conflicts with the multi-thread feature and the reference-transparency basis. 
    Besides, there's huge works if trying to implement some mutable structures. I'm not sure if I could keep this project on... Let me learn some other PLT knowledge first...