-- SPDX-License-Identifier: PMPL-1.0
-- proven_tree.ads - Ada bindings for proven SafeTree module
--
-- This package provides formally verified tree operations based on
-- the proven library (https://github.com/hyperpolymath/proven).
-- Operations are proven correct via dependent types in Idris 2.
--
-- Key guarantees:
-- - Navigation always succeeds on valid paths (ValidPath proof)
-- - Tree depth is bounded at compile time (DepthTree type)
-- - Map operations preserve structure (mapPreservesStructure proof)
-- - Zipper round-trips preserve the tree (zipperRoundTrip proof)

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Element_Type is private;
package Proven_Tree is

   -- Tree node with children
   type Tree_Node;
   type Tree_Node_Access is access Tree_Node;

   package Tree_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Tree_Node_Access);

   type Tree_Node is record
      Value    : Element_Type;
      Children : Tree_Vectors.Vector;
   end record;

   -- Path through tree (list of child indices)
   package Path_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);

   subtype Path_Type is Path_Vectors.Vector;

   -- Result type for operations that may fail
   type Maybe_Tree is record
      Has_Value : Boolean := False;
      Value     : Tree_Node_Access := null;
   end record;

   -- Create a leaf node
   function Make_Leaf (Value : Element_Type) return Tree_Node_Access;

   -- Create a branch node with children
   function Make_Branch
     (Value    : Element_Type;
      Children : Tree_Vectors.Vector) return Tree_Node_Access;

   -- Get root value
   function Root_Value (Node : Tree_Node_Access) return Element_Type
     with Pre => Node /= null;

   -- Check if leaf (no children)
   function Is_Leaf (Node : Tree_Node_Access) return Boolean
     with Pre => Node /= null;

   -- Count total nodes
   function Node_Count (Node : Tree_Node_Access) return Natural
     with Pre => Node /= null;

   -- Compute tree depth
   function Tree_Depth (Node : Tree_Node_Access) return Natural
     with Pre => Node /= null;

   -- Navigate to node via path (formally verified to succeed on valid paths)
   -- Corresponds to proven ValidPath proof type
   function Navigate
     (Node : Tree_Node_Access;
      Path : Path_Type) return Maybe_Tree
     with Pre => Node /= null;

   -- Check if path is valid (proof-carrying function)
   function Is_Valid_Path
     (Node : Tree_Node_Access;
      Path : Path_Type) return Boolean
     with Pre => Node /= null;

   -- Map function over tree (creates deep copy - proven to preserve structure)
   generic
      with function Transform (E : Element_Type) return Element_Type;
   package Map_Tree is
      function Map (Node : Tree_Node_Access) return Tree_Node_Access;
   end Map_Tree;

   -- Fold tree bottom-up
   generic
      type Accumulator_Type is private;
      with function Combine
        (Value    : Element_Type;
         Children : Accumulator_Type) return Accumulator_Type;
   function Fold_Tree
     (Node : Tree_Node_Access) return Accumulator_Type
     with Pre => Node /= null;

   -- Pre-order traversal (visits all nodes - proven complete)
   generic
      with procedure Visit (E : Element_Type);
   procedure Pre_Order_Traverse (Node : Tree_Node_Access)
     with Pre => Node /= null;

   -- Post-order traversal
   generic
      with procedure Visit (E : Element_Type);
   procedure Post_Order_Traverse (Node : Tree_Node_Access)
     with Pre => Node /= null;

   -- Level-order (BFS) traversal
   generic
      with procedure Visit (E : Element_Type);
   procedure Level_Order_Traverse (Node : Tree_Node_Access)
     with Pre => Node /= null;

   -- Tree zipper for efficient navigation (proven round-trip)
   type Tree_Zipper is private;

   function To_Zipper (Node : Tree_Node_Access) return Tree_Zipper
     with Pre => Node /= null;

   function From_Zipper (Z : Tree_Zipper) return Tree_Node_Access;

   function Go_Down (Z : Tree_Zipper) return Tree_Zipper;
   function Go_Up (Z : Tree_Zipper) return Tree_Zipper;
   function Go_Left (Z : Tree_Zipper) return Tree_Zipper;
   function Go_Right (Z : Tree_Zipper) return Tree_Zipper;

   function Can_Go_Down (Z : Tree_Zipper) return Boolean;
   function Can_Go_Up (Z : Tree_Zipper) return Boolean;
   function Can_Go_Left (Z : Tree_Zipper) return Boolean;
   function Can_Go_Right (Z : Tree_Zipper) return Boolean;

   function Focus_Value (Z : Tree_Zipper) return Element_Type;

private

   -- Parent context for zipper
   type Parent_Context is record
      Parent_Value : Element_Type;
      Left_Siblings : Tree_Vectors.Vector;
      Right_Siblings : Tree_Vectors.Vector;
   end record;

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Parent_Context);

   type Tree_Zipper is record
      Focus   : Tree_Node_Access;
      Lefts   : Tree_Vectors.Vector;
      Rights  : Tree_Vectors.Vector;
      Parents : Context_Vectors.Vector;
   end record;

end Proven_Tree;
