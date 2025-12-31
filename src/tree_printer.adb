-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Tree export engine implementation
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Terminal;
with File_Types; use File_Types;

package body Tree_Printer is

   -- Box drawing characters
   Branch    : constant String := "├── ";
   Last_Item : constant String := "└── ";
   Vertical  : constant String := "│   ";
   Space     : constant String := "    ";

   function Default_Options return Export_Options is
   begin
      return Export_Options'(
         Output_File    => Null_Unbounded_String,
         Root_Dir       => To_Unbounded_String (Current_Directory),
         Max_Depth      => 10,
         Show_Hidden    => False,
         Show_Size      => False,
         No_Files       => False,
         No_Dirs        => False,
         Only_Type      => File_Types.Regular_File,
         Filter_By_Type => False,
         Exclude_Dirs   => String_Vectors.Empty_Vector,
         Exclude_Files  => String_Vectors.Empty_Vector
      );
   end Default_Options;

   procedure Parse_List
     (Input  : String;
      Result : out String_Vectors.Vector)
   is
      Start : Positive := Input'First;
      Pos   : Natural;
   begin
      Result := String_Vectors.Empty_Vector;
      if Input'Length = 0 then
         return;
      end if;

      loop
         Pos := Ada.Strings.Fixed.Index (Input (Start .. Input'Last), ",");
         if Pos = 0 then
            Result.Append (To_Unbounded_String (Input (Start .. Input'Last)));
            exit;
         else
            Result.Append (To_Unbounded_String (Input (Start .. Pos - 1)));
            Start := Pos + 1;
         end if;
         exit when Start > Input'Last;
      end loop;
   end Parse_List;

   function Matches_Pattern
     (Name     : String;
      Patterns : String_Vectors.Vector) return Boolean
   is
   begin
      for Pattern of Patterns loop
         declare
            P : constant String := To_String (Pattern);
         begin
            -- Simple matching: exact match or wildcard prefix match
            if P = Name then
               return True;
            elsif P'Length > 1 and then P (P'First) = '*' then
               -- Suffix match (e.g., "*.txt")
               declare
                  Suffix : constant String := P (P'First + 1 .. P'Last);
               begin
                  if Name'Length >= Suffix'Length and then
                     Name (Name'Last - Suffix'Length + 1 .. Name'Last) = Suffix
                  then
                     return True;
                  end if;
               end;
            end if;
         end;
      end loop;
      return False;
   end Matches_Pattern;

   function Format_Size (Size : Long_Long_Integer) return String is
      KB : constant Long_Long_Integer := 1024;
      MB : constant Long_Long_Integer := KB * 1024;
      GB : constant Long_Long_Integer := MB * 1024;
   begin
      if Size >= GB then
         return Long_Long_Integer'Image (Size / GB) & " GB";
      elsif Size >= MB then
         return Long_Long_Integer'Image (Size / MB) & " MB";
      elsif Size >= KB then
         return Long_Long_Integer'Image (Size / KB) & " KB";
      else
         return Long_Long_Integer'Image (Size) & " B";
      end if;
   end Format_Size;

   function Timestamp return String is
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return Ada.Calendar.Formatting.Image (Now);
   end Timestamp;

   -- Internal recursive procedure for tree traversal
   procedure Traverse_Tree
     (Dir_Path : String;
      Options  : Export_Options;
      Stats    : in out Tree_Statistics;
      Depth    : Natural;
      Prefix   : String;
      Output   : File_Type)
   is
      Search     : Search_Type;
      Entry_Info : Directory_Entry_Type;
      Entries    : String_Vectors.Vector;
      Index      : Natural := 0;
   begin
      if Depth > Options.Max_Depth then
         return;
      end if;

      -- Collect entries first to know which is last
      begin
         Start_Search (Search, Dir_Path, "");
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Entry_Info);
            declare
               Name : constant String := Simple_Name (Entry_Info);
            begin
               -- Skip . and ..
               if Name /= "." and then Name /= ".." then
                  -- Check hidden file filter
                  if not Options.Show_Hidden and then Is_Hidden (Name) then
                     null;  -- Skip hidden
                  else
                     Entries.Append (To_Unbounded_String (Name));
                  end if;
               end if;
            end;
         end loop;
         End_Search (Search);
      exception
         when Ada.Directories.Use_Error =>
            Stats.Errors := Stats.Errors + 1;
            Put_Line (Output, Prefix & "[Permission Denied]");
            return;
      end;

      -- Process entries
      for I in Entries.First_Index .. Entries.Last_Index loop
         declare
            Name      : constant String := To_String (Entries (I));
            Full_Path : constant String := Dir_Path & "/" & Name;
            Is_Last   : constant Boolean := I = Entries.Last_Index;
            Cat       : File_Types.Category;
            Item_Prefix : constant String :=
               (if Is_Last then Last_Item else Branch);
            Next_Prefix : constant String :=
               Prefix & (if Is_Last then Space else Vertical);
         begin
            -- Get file category
            if Exists (Full_Path) then
               Cat := File_Types.Detect (Full_Path);
            else
               goto Continue;
            end if;

            -- Apply filters
            if Cat = File_Types.Directory then
               -- Check directory exclusion
               if Matches_Pattern (Name, Options.Exclude_Dirs) then
                  Stats.Excluded_Dirs := Stats.Excluded_Dirs + 1;
                  goto Continue;
               end if;

               if Options.No_Dirs then
                  goto Continue;
               end if;

               Stats.Total_Dirs := Stats.Total_Dirs + 1;

               -- Print directory
               Put (Output, Prefix & Item_Prefix);
               Put_Line (Output, Icon_For (Cat) & " " & Name);

               -- Recurse
               Traverse_Tree (Full_Path, Options, Stats, Depth + 1,
                              Next_Prefix, Output);
            else
               -- Check file exclusion
               if Matches_Pattern (Name, Options.Exclude_Files) then
                  Stats.Excluded_Files := Stats.Excluded_Files + 1;
                  goto Continue;
               end if;

               if Options.No_Files then
                  goto Continue;
               end if;

               -- Check type filter
               if Options.Filter_By_Type and then Cat /= Options.Only_Type then
                  goto Continue;
               end if;

               Stats.Total_Files := Stats.Total_Files + 1;

               -- Get size if needed
               if Options.Show_Size then
                  declare
                     File_Size : constant Long_Long_Integer :=
                        Long_Long_Integer (Ada.Directories.Size (Full_Path));
                  begin
                     Stats.Total_Size := Stats.Total_Size + File_Size;
                     Put (Output, Prefix & Item_Prefix);
                     Put_Line (Output, Icon_For (Cat) & " " & Name &
                               " (" & Format_Size (File_Size) & ")");
                  end;
               else
                  Put (Output, Prefix & Item_Prefix);
                  Put_Line (Output, Icon_For (Cat) & " " & Name);
               end if;
            end if;

            <<Continue>>
         end;
      end loop;
   end Traverse_Tree;

   procedure Export_Tree
     (Options : Export_Options;
      Stats   : out Tree_Statistics)
   is
      Output   : File_Type;
      Root_Dir : constant String := To_String (Options.Root_Dir);
   begin
      Stats := (others => 0);

      Create (Output, Out_File, To_String (Options.Output_File));

      -- Write header
      Put_Line (Output, "Directory tree: " & Root_Dir);
      Put_Line (Output, "Generated: " & Timestamp);
      Put_Line (Output, "Max depth: " & Options.Max_Depth'Image);
      Put_Line (Output, String'(1 .. 80 => '='));
      Put_Line (Output, "");

      -- Print root
      Put_Line (Output, File_Types.Icon_For (File_Types.Directory) & " " &
                Simple_Name (Root_Dir));

      -- Traverse tree
      Traverse_Tree (Root_Dir, Options, Stats, 1, "", Output);

      -- Write statistics
      Put_Line (Output, "");
      Put_Line (Output, String'(1 .. 80 => '='));
      Put_Line (Output, "Statistics:");
      Put_Line (Output, "  Directories:  " & Stats.Total_Dirs'Image);
      Put_Line (Output, "  Files:        " & Stats.Total_Files'Image);
      if Options.Show_Size then
         Put_Line (Output, "  Total size:   " & Format_Size (Stats.Total_Size));
      end if;
      if Stats.Excluded_Dirs > 0 then
         Put_Line (Output, "  Excluded dirs: " & Stats.Excluded_Dirs'Image);
      end if;
      if Stats.Excluded_Files > 0 then
         Put_Line (Output, "  Excluded files:" & Stats.Excluded_Files'Image);
      end if;
      if Stats.Errors > 0 then
         Put_Line (Output, "  Errors:       " & Stats.Errors'Image);
      end if;

      Close (Output);

      Terminal.Success ("Exported to: " & To_String (Options.Output_File));
   end Export_Tree;

   procedure Print_Tree
     (Options : Export_Options;
      Stats   : out Tree_Statistics)
   is
      Root_Dir : constant String := To_String (Options.Root_Dir);
   begin
      Stats := (others => 0);

      -- Write header
      Terminal.Show_Header ("Directory tree: " & Root_Dir);
      Put_Line ("Generated: " & Timestamp);
      Put_Line ("Max depth: " & Options.Max_Depth'Image);
      Put_Line (String'(1 .. 80 => '='));
      Put_Line ("");

      -- Print root
      Put_Line (File_Types.Icon_For (File_Types.Directory) & " " &
                Simple_Name (Root_Dir));

      -- Traverse tree (to standard output)
      Traverse_Tree (Root_Dir, Options, Stats, 1, "", Standard_Output);

      -- Write statistics
      Put_Line ("");
      Put_Line (String'(1 .. 80 => '='));
      Put_Line ("Statistics:");
      Put_Line ("  Directories:  " & Stats.Total_Dirs'Image);
      Put_Line ("  Files:        " & Stats.Total_Files'Image);
      if Options.Show_Size then
         Put_Line ("  Total size:   " & Format_Size (Stats.Total_Size));
      end if;
   end Print_Tree;

end Tree_Printer;
