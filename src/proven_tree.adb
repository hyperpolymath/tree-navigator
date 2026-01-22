-- SPDX-License-Identifier: PMPL-1.0
-- proven_tree.adb - Ada implementation of proven SafeTree bindings

pragma Ada_2022;

package body Proven_Tree is

   use Tree_Vectors;
   use Context_Vectors;

   function Make_Leaf (Value : Element_Type) return Tree_Node_Access is
   begin
      return new Tree_Node'(Value => Value, Children => Tree_Vectors.Empty_Vector);
   end Make_Leaf;

   function Make_Branch
     (Value    : Element_Type;
      Children : Tree_Vectors.Vector) return Tree_Node_Access is
   begin
      return new Tree_Node'(Value => Value, Children => Children);
   end Make_Branch;

   function Root_Value (Node : Tree_Node_Access) return Element_Type is
   begin
      return Node.Value;
   end Root_Value;

   function Is_Leaf (Node : Tree_Node_Access) return Boolean is
   begin
      return Node.Children.Is_Empty;
   end Is_Leaf;

   function Node_Count (Node : Tree_Node_Access) return Natural is
      Count : Natural := 1;
   begin
      for Child of Node.Children loop
         Count := Count + Node_Count (Child);
      end loop;
      return Count;
   end Node_Count;

   function Tree_Depth (Node : Tree_Node_Access) return Natural is
      Max_Child_Depth : Natural := 0;
   begin
      if Node.Children.Is_Empty then
         return 0;
      end if;
      for Child of Node.Children loop
         declare
            Child_Depth : constant Natural := Tree_Depth (Child);
         begin
            if Child_Depth > Max_Child_Depth then
               Max_Child_Depth := Child_Depth;
            end if;
         end;
      end loop;
      return 1 + Max_Child_Depth;
   end Tree_Depth;

   function Navigate
     (Node : Tree_Node_Access;
      Path : Path_Type) return Maybe_Tree is
      Current : Tree_Node_Access := Node;
   begin
      for Index of Path loop
         if Index >= Natural (Current.Children.Length) then
            return (Has_Value => False, Value => null);
         end if;
         Current := Current.Children.Element (Index);
      end loop;
      return (Has_Value => True, Value => Current);
   end Navigate;

   function Is_Valid_Path
     (Node : Tree_Node_Access;
      Path : Path_Type) return Boolean is
   begin
      return Navigate (Node, Path).Has_Value;
   end Is_Valid_Path;

   -- Map over tree, transforming each value
   package body Map_Tree is
      function Map (Node : Tree_Node_Access) return Tree_Node_Access is
         New_Children : Tree_Vectors.Vector;
      begin
         for Child of Node.Children loop
            New_Children.Append (Map (Child));
         end loop;
         return new Tree_Node'(
            Value    => Transform (Node.Value),
            Children => New_Children);
      end Map;
   end Map_Tree;

   function Fold_Tree
     (Node : Tree_Node_Access) return Accumulator_Type is
      Child_Results : Accumulator_Type;
   begin
      -- For simplicity, fold children left-to-right
      for Child of Node.Children loop
         Child_Results := Combine (Child.Value, Fold_Tree (Child));
      end loop;
      return Combine (Node.Value, Child_Results);
   end Fold_Tree;

   procedure Pre_Order_Traverse (Node : Tree_Node_Access) is
   begin
      Visit (Node.Value);
      for Child of Node.Children loop
         Pre_Order_Traverse (Child);
      end loop;
   end Pre_Order_Traverse;

   procedure Post_Order_Traverse (Node : Tree_Node_Access) is
   begin
      for Child of Node.Children loop
         Post_Order_Traverse (Child);
      end loop;
      Visit (Node.Value);
   end Post_Order_Traverse;

   procedure Level_Order_Traverse (Node : Tree_Node_Access) is
      -- Simple BFS using vector as queue
      Queue : Tree_Vectors.Vector;
   begin
      Queue.Append (Node);
      while not Queue.Is_Empty loop
         declare
            Current : constant Tree_Node_Access := Queue.First_Element;
         begin
            Queue.Delete_First;
            Visit (Current.Value);
            for Child of Current.Children loop
               Queue.Append (Child);
            end loop;
         end;
      end loop;
   end Level_Order_Traverse;

   -- Zipper implementation

   function To_Zipper (Node : Tree_Node_Access) return Tree_Zipper is
   begin
      return (Focus   => Node,
              Lefts   => Tree_Vectors.Empty_Vector,
              Rights  => Tree_Vectors.Empty_Vector,
              Parents => Context_Vectors.Empty_Vector);
   end To_Zipper;

   function From_Zipper (Z : Tree_Zipper) return Tree_Node_Access is
      Result : Tree_Zipper := Z;
   begin
      -- Go up to root
      while not Result.Parents.Is_Empty loop
         Result := Go_Up (Result);
      end loop;
      return Result.Focus;
   end From_Zipper;

   function Can_Go_Down (Z : Tree_Zipper) return Boolean is
   begin
      return not Z.Focus.Children.Is_Empty;
   end Can_Go_Down;

   function Can_Go_Up (Z : Tree_Zipper) return Boolean is
   begin
      return not Z.Parents.Is_Empty;
   end Can_Go_Up;

   function Can_Go_Left (Z : Tree_Zipper) return Boolean is
   begin
      return not Z.Lefts.Is_Empty;
   end Can_Go_Left;

   function Can_Go_Right (Z : Tree_Zipper) return Boolean is
   begin
      return not Z.Rights.Is_Empty;
   end Can_Go_Right;

   function Go_Down (Z : Tree_Zipper) return Tree_Zipper is
      First_Child : Tree_Node_Access;
      New_Rights  : Tree_Vectors.Vector;
   begin
      if Z.Focus.Children.Is_Empty then
         return Z;
      end if;

      First_Child := Z.Focus.Children.First_Element;

      -- Rest of children become right siblings
      for I in 1 .. Natural (Z.Focus.Children.Length) - 1 loop
         New_Rights.Append (Z.Focus.Children.Element (I));
      end loop;

      return (Focus   => First_Child,
              Lefts   => Tree_Vectors.Empty_Vector,
              Rights  => New_Rights,
              Parents => Z.Parents & Parent_Context'(
                           Parent_Value   => Z.Focus.Value,
                           Left_Siblings  => Z.Lefts,
                           Right_Siblings => Z.Rights));
   end Go_Down;

   function Go_Up (Z : Tree_Zipper) return Tree_Zipper is
      Parent : Parent_Context;
      All_Children : Tree_Vectors.Vector;
   begin
      if Z.Parents.Is_Empty then
         return Z;
      end if;

      Parent := Z.Parents.Last_Element;

      -- Reconstruct children: lefts (reversed) + focus + rights
      for I in reverse 0 .. Natural (Z.Lefts.Length) - 1 loop
         All_Children.Append (Z.Lefts.Element (I));
      end loop;
      All_Children.Append (Z.Focus);
      for R of Z.Rights loop
         All_Children.Append (R);
      end loop;

      declare
         New_Parents : Context_Vectors.Vector := Z.Parents;
      begin
         New_Parents.Delete_Last;
         return (Focus   => new Tree_Node'(
                             Value    => Parent.Parent_Value,
                             Children => All_Children),
                 Lefts   => Parent.Left_Siblings,
                 Rights  => Parent.Right_Siblings,
                 Parents => New_Parents);
      end;
   end Go_Up;

   function Go_Left (Z : Tree_Zipper) return Tree_Zipper is
   begin
      if Z.Lefts.Is_Empty then
         return Z;
      end if;

      declare
         Left_Sibling : constant Tree_Node_Access := Z.Lefts.Last_Element;
         New_Lefts    : Tree_Vectors.Vector := Z.Lefts;
      begin
         New_Lefts.Delete_Last;
         return (Focus   => Left_Sibling,
                 Lefts   => New_Lefts,
                 Rights  => Z.Focus & Z.Rights,
                 Parents => Z.Parents);
      end;
   end Go_Left;

   function Go_Right (Z : Tree_Zipper) return Tree_Zipper is
   begin
      if Z.Rights.Is_Empty then
         return Z;
      end if;

      declare
         Right_Sibling : constant Tree_Node_Access := Z.Rights.First_Element;
         New_Rights    : Tree_Vectors.Vector := Z.Rights;
      begin
         New_Rights.Delete_First;
         return (Focus   => Right_Sibling,
                 Lefts   => Z.Lefts & Z.Focus,
                 Rights  => New_Rights,
                 Parents => Z.Parents);
      end;
   end Go_Right;

   function Focus_Value (Z : Tree_Zipper) return Element_Type is
   begin
      return Z.Focus.Value;
   end Focus_Value;

end Proven_Tree;
