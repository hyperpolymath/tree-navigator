-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Tree export engine for directory visualization
pragma Ada_2022;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Config;
with File_Types;

package Tree_Printer is

   -- String vector for exclusion lists
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);

   -- Export options record
   type Export_Options is record
      Output_File    : Unbounded_String := Null_Unbounded_String;
      Root_Dir       : Unbounded_String := Null_Unbounded_String;
      Max_Depth      : Positive := 10;
      Show_Hidden    : Boolean := False;
      Show_Size      : Boolean := False;
      No_Files       : Boolean := False;
      No_Dirs        : Boolean := False;
      Only_Type      : File_Types.Category := File_Types.Regular_File;
      Filter_By_Type : Boolean := False;
      Exclude_Dirs   : String_Vectors.Vector;
      Exclude_Files  : String_Vectors.Vector;
   end record;

   -- Statistics record
   type Tree_Statistics is record
      Total_Dirs     : Natural := 0;
      Total_Files    : Natural := 0;
      Total_Size     : Long_Long_Integer := 0;
      Excluded_Dirs  : Natural := 0;
      Excluded_Files : Natural := 0;
      Errors         : Natural := 0;
   end record;

   -- Initialize export options with defaults
   function Default_Options return Export_Options;

   -- Parse comma-separated list into vector
   procedure Parse_List
     (Input  : String;
      Result : out String_Vectors.Vector);

   -- Check if name matches any pattern in list
   function Matches_Pattern
     (Name     : String;
      Patterns : String_Vectors.Vector) return Boolean;

   -- Export tree to file
   procedure Export_Tree
     (Options : Export_Options;
      Stats   : out Tree_Statistics);

   -- Export tree to standard output
   procedure Print_Tree
     (Options : Export_Options;
      Stats   : out Tree_Statistics);

   -- Format file size for display
   function Format_Size (Size : Long_Long_Integer) return String;

   -- Get current timestamp string
   function Timestamp return String;

end Tree_Printer;
