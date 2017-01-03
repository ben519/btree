# btree

The btree package makes it easy to work with binary tree structures as data.table objects.

## What is a btree?

For our purposes, it's a data.table with the following structure:

- Each row represents a node in the tree and is uniquely identified by NodeID
- Each row contains a ParentNodeID which points at the node's parent. (The root node has ParentID = NA)

Using the `make_btree()` constructor generates a data.table with additional properties. Namely:

- Each row contains a IsLeaf attribute indicating if the node is a leaf
- Each row contains a LeftChildID and RightChildID (NA for leaf nodes)
- Each row contains a Path attribute which is a string of 0s and 1s representing the path from the root 
- node to the given node.  E.g. "010" = root -> left child -> right child -> left child

## Decision Tree / Regression Tree

A btree can also be a decision tree / regression tree where:

- Each non-leaf node must contain a decision rule determining how to split the data
- Rules are represents by the attribute ConditionLeft, such that evaluating to TRUE sends a point to the left child node
- E.g. ConditionLeft could be "Petal.Length <= 2.35" or in the case of xgboost, "Petal.Length <= 2.35 | is.na(Petal.Length)"
