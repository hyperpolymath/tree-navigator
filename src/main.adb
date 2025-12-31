-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Tree Navigator - Directory tree visualization and export tool
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal;
with Ada.Exceptions;
with Config;
with Bookmarks;
with Navigator;
with Tree_Printer;
with File_Types;

procedure Main is
   Cfg : Config.Configuration;
   Paths : Config.File_Paths;
   State : Navigator.Navigation_State;
   BM_Map : Bookmarks.Bookmark_Map;

   -- Export mode flag
   Export_Mode : Boolean := False;
   Export_Opts : Tree_Printer.Export_Options := Tree_Printer.Default_Options;

   -- Initialize file paths
   procedure Initialize_Paths is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
   begin
      Paths.Config_Dir := new String'(Home & "/.config/tree-navigator");
      Paths.Cache_Dir := new String'(Home & "/.cache/tree-navigator");
      Paths.Bookmark_File := new String'(Paths.Config_Dir.all & "/bookmarks.txt");
      Paths.History_File := new String'(Paths.Config_Dir.all & "/history.txt");

      -- Ensure directories exist
      if not Ada.Directories.Exists (Paths.Config_Dir.all) then
         Ada.Directories.Create_Path (Paths.Config_Dir.all);
      end if;
      if not Ada.Directories.Exists (Paths.Cache_Dir.all) then
         Ada.Directories.Create_Path (Paths.Cache_Dir.all);
      end if;
   end Initialize_Paths;

   -- Show help
   procedure Show_Help is
   begin
      Terminal.Show_Header ("Tree Navigator v2.1 (Ada 2022)");
      Put_Line ("");
      Put_Line ("USAGE:");
      Put_Line ("    tn [OPTIONS] [DEPTH]");
      Put_Line ("    treenav [OPTIONS]");
      Put_Line ("    tree-navigator [OPTIONS]");
      Put_Line ("");
      Put_Line ("GENERAL OPTIONS:");
      Put_Line ("    -h, --help           Show this help");
      Put_Line ("    -v, --version        Show version");
      Put_Line ("    --no-color           Disable colored output");
      Put_Line ("    --verbose            Enable verbose output");
      Put_Line ("");
      Put_Line ("INTERACTIVE MODE:");
      Put_Line ("    -d, --depth N        Set maximum navigation depth");
      Put_Line ("");
      Put_Line ("EXPORT MODE:");
      Put_Line ("    --export FILE        Export tree to file");
      Put_Line ("    --output FILE        Alternative to --export");
      Put_Line ("    --dir PATH           Root directory (default: current)");
      Put_Line ("    --max-depth N        Maximum depth (default: 10)");
      Put_Line ("    --show-hidden        Include hidden files");
      Put_Line ("    --show-size          Display file sizes");
      Put_Line ("    --no-files           Only show directories");
      Put_Line ("    --no-dirs            Only show files");
      Put_Line ("    --exclude-dirs LIST  Comma-separated dirs to exclude");
      Put_Line ("    --exclude-files LIST Comma-separated files to exclude");
      Put_Line ("    --only-type TYPE     Filter by type:");
      Put_Line ("                         executable, config, script, data, hidden");
      Put_Line ("");
      Put_Line ("EXAMPLES:");
      Put_Line ("    tn                           # Interactive mode");
      Put_Line ("    tn 5                         # Navigate to depth 5");
      Put_Line ("    tn --export tree.txt         # Export to file");
      Put_Line ("    tn --export out.txt --exclude-dirs target,node_modules");
      Put_Line ("    tn --export bins.txt --only-type executable");
      Put_Line ("");
   end Show_Help;

   -- Parse file type string
   function Parse_File_Type (S : String) return File_Types.Category is
   begin
      if S = "executable" then
         return File_Types.Executable;
      elsif S = "config" then
         return File_Types.System_Config;
      elsif S = "script" then
         return File_Types.Shell_Script;
      elsif S = "data" then
         return File_Types.Data_File;
      elsif S = "hidden" then
         return File_Types.Hidden_File;
      elsif S = "directory" then
         return File_Types.Directory;
      else
         return File_Types.Regular_File;
      end if;
   end Parse_File_Type;

   -- Parse command line arguments
   procedure Parse_Arguments is
      I : Positive := 1;
      Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      while I <= Arg_Count loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (I);
         begin
            if Arg = "-h" or Arg = "--help" then
               Show_Help;
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
               return;

            elsif Arg = "-v" or Arg = "--version" then
               Put_Line ("tree-navigator 2.1.0 (Ada 2022)");
               Put_Line ("Aliases: tn, treenav");
               return;

            elsif Arg = "--no-color" then
               Cfg.Color_Mode := False;

            elsif Arg = "--verbose" then
               Cfg.Verbose := True;

            elsif Arg = "-d" or Arg = "--depth" or Arg = "--max-depth" then
               I := I + 1;
               if I <= Arg_Count then
                  declare
                     Depth_Val : constant Positive :=
                        Positive'Value (Ada.Command_Line.Argument (I));
                  begin
                     Cfg.Max_Depth := Depth_Val;
                     Export_Opts.Max_Depth := Depth_Val;
                  end;
               end if;

            elsif Arg = "--export" or Arg = "--output" then
               Export_Mode := True;
               I := I + 1;
               if I <= Arg_Count then
                  Export_Opts.Output_File :=
                     To_Unbounded_String (Ada.Command_Line.Argument (I));
               end if;

            elsif Arg = "--dir" then
               I := I + 1;
               if I <= Arg_Count then
                  Export_Opts.Root_Dir :=
                     To_Unbounded_String (Ada.Command_Line.Argument (I));
               end if;

            elsif Arg = "--show-hidden" then
               Cfg.Show_Hidden := True;
               Export_Opts.Show_Hidden := True;

            elsif Arg = "--show-size" then
               Export_Opts.Show_Size := True;

            elsif Arg = "--no-files" then
               Export_Opts.No_Files := True;

            elsif Arg = "--no-dirs" then
               Export_Opts.No_Dirs := True;

            elsif Arg = "--exclude-dirs" then
               I := I + 1;
               if I <= Arg_Count then
                  Tree_Printer.Parse_List
                    (Ada.Command_Line.Argument (I), Export_Opts.Exclude_Dirs);
               end if;

            elsif Arg = "--exclude-files" then
               I := I + 1;
               if I <= Arg_Count then
                  Tree_Printer.Parse_List
                    (Ada.Command_Line.Argument (I), Export_Opts.Exclude_Files);
               end if;

            elsif Arg = "--only-type" then
               I := I + 1;
               if I <= Arg_Count then
                  Export_Opts.Only_Type :=
                     Parse_File_Type (Ada.Command_Line.Argument (I));
                  Export_Opts.Filter_By_Type := True;
               end if;

            else
               -- Try to parse as depth number
               begin
                  Cfg.Max_Depth := Positive'Value (Arg);
                  Export_Opts.Max_Depth := Cfg.Max_Depth;
               exception
                  when Constraint_Error =>
                     Terminal.Error ("Unknown option or invalid depth: " & Arg);
                     Terminal.Info ("Use --help for usage information");
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                     return;
               end;
            end if;
         end;
         I := I + 1;
      end loop;
   end Parse_Arguments;

begin
   -- Initialize
   Initialize_Paths;
   Config.Load (Cfg, Paths);

   -- Parse arguments
   Parse_Arguments;

   -- Check if early exit (help/version)
   if Ada.Command_Line.Exit_Status = Ada.Command_Line.Failure then
      return;
   end if;

   -- Validate configuration
   if not Config.Is_Valid (Cfg) then
      Terminal.Error ("Invalid configuration");
      return;
   end if;

   if Export_Mode then
      -- Export mode
      if Length (Export_Opts.Output_File) = 0 then
         Terminal.Error ("Export mode requires --export FILE");
         return;
      end if;

      declare
         Stats : Tree_Printer.Tree_Statistics;
      begin
         Tree_Printer.Export_Tree (Export_Opts, Stats);
         Terminal.Info ("Directories: " & Stats.Total_Dirs'Image);
         Terminal.Info ("Files:       " & Stats.Total_Files'Image);
      end;
   else
      -- Interactive mode
      Bookmarks.Load (BM_Map, Paths.Bookmark_File.all);

      -- Initialize navigator
      State := Navigator.Initialize (Cfg);

      -- Start interactive navigation
      Terminal.Info ("Starting navigation (max depth:" & Cfg.Max_Depth'Image & ")");
      Terminal.Info ("Press 'q' to quit at any time");
      Put_Line ("");

      Navigator.Navigate_Interactive (State, BM_Map);

      -- Save bookmarks
      Bookmarks.Save (BM_Map, Paths.Bookmark_File.all);

      -- Save configuration
      Config.Save (Cfg, Paths);

      Terminal.Success ("Navigation complete!");
      Put_Line ("Final location: " & Navigator.Current_Directory (State));
   end if;

exception
   when E : others =>
      Terminal.Error ("Fatal error occurred");
      Terminal.Error (Ada.Exceptions.Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;
