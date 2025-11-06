pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
with Terminal;
with Ada.Exceptions;
with Config;
with Bookmarks;
with Navigator;

procedure Main is
   Cfg : Config.Configuration;
   Paths : Config.File_Paths;
   State : Navigator.Navigation_State;
   BM_Map : Bookmarks.Bookmark_Map;

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
      Terminal.Show_Header ("Enhanced Tree Navigator v2.0 (Ada Edition)")
      Put_Line ("");
      Put_Line ("USAGE:");
      Put_Line ("    tree-navigator [OPTIONS] [DEPTH]");
      Put_Line ("");
      Put_Line ("OPTIONS:");
      Put_Line ("    -h, --help       Show this help");
      Put_Line ("    -v, --version    Show version");
      Put_Line ("    -d, --depth N    Set maximum depth");
      Put_Line ("    --no-color       Disable colored output");
      Put_Line ("    --verbose        Enable verbose output");
      Put_Line ("");
      Put_Line ("EXAMPLES:");
      Put_Line ("    tree-navigator         # Interactive mode with default depth");
      Put_Line ("    tree-navigator 5       # Navigate to depth 5");
      Put_Line ("    tree-navigator -d 10   # Navigate to depth 10");
      Put_Line ("");
   end Show_Help;

begin

   -- Initialize
   Initialize_Paths;
   Config.Load (Cfg, Paths);
   Bookmarks.Load (BM_Map, Paths.Bookmark_File.all);

   -- Parse arguments
   if Ada.Command_Line.Argument_Count > 0 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         if Arg = "-h" or Arg = "--help" then
            Show_Help;
            return;
         elsif Arg = "-v" or Arg = "--version" then
            Put_Line ("tree-navigator 2.0.0 (Ada 2022)");
            return;
         elsif Arg = "--no-color" then
            Cfg.Color_Mode := False;
         elsif Arg = "--verbose" then
            Cfg.Verbose := True;
         elsif Arg = "-d" or Arg = "--depth" then
            if Ada.Command_Line.Argument_Count > 1 then
               Cfg.Max_Depth := Positive'Value (Ada.Command_Line.Argument (2));
            end if;
         else
            -- Try to parse as depth number
            begin
               Cfg.Max_Depth := Positive'Value (Arg);
            exception
               when Constraint_Error =>
                  Terminal.Error ("Invalid depth: " & Arg);
                  return;
            end;
         end if;
      end;
   end if;

   -- Validate configuration
   if not Config.Is_Valid (Cfg) then
      Terminal.Error ("Invalid configuration");
      return;
   end if;

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

exception
   when E : others =>
      Terminal.Error ("Fatal error occurred");
      Terminal.Error (Ada.Exceptions.Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;