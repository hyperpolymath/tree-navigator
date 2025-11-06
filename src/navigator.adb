pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Terminal;
with File_Types; use File_Types;

package body Navigator is

   function Initialize (Cfg : Config.Configuration) return Navigation_State is
      Current : constant String := Current_Directory;
   begin
      return Navigation_State'(
         Cfg              => Cfg,
         Current_Path     => To_Unbounded_String (Current),
         Navigation_Stack => String_Vectors.Empty_Vector,
         Current_Depth    => 0
      );
   end Initialize;

   procedure Change_Directory
     (State : in out Navigation_State;
      Path  : String)
   is
   begin
      -- Save current path to stack
      State.Navigation_Stack.Append (State.Current_Path);

      -- Change directory
      Set_Directory (Path);
      State.Current_Path := To_Unbounded_String (Path);
      State.Current_Depth := State.Current_Depth + 1;

      Terminal.Debug ("Changed to: " & Path, State.Cfg.Verbose);
   end Change_Directory;

   procedure Go_Up (State : in out Navigation_State) is
   begin
      if not State.Navigation_Stack.Is_Empty then
         declare
            Previous : constant Unbounded_String := State.Navigation_Stack.Last_Element;
         begin
            State.Navigation_Stack.Delete_Last;
            Set_Directory (To_String (Previous));
            State.Current_Path := Previous;
            State.Current_Depth := Natural'Max (0, State.Current_Depth - 1);
         end;
      else
         -- Just go up one level
         declare
            Parent : constant String := Containing_Directory (To_String (State.Current_Path));
         begin
            if Parent /= To_String (State.Current_Path) then
               Set_Directory (Parent);
               State.Current_Path := To_Unbounded_String (Parent);
               State.Current_Depth := Natural'Max (0, State.Current_Depth - 1);
            end if;
         end;
      end if;
   end Go_Up;

   procedure Go_Home (State : in out Navigation_State) is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
   begin
      State.Navigation_Stack.Clear;
      Set_Directory (Home);
      State.Current_Path := To_Unbounded_String (Home);
      State.Current_Depth := 0;
   end Go_Home;

   procedure List_Contents
     (State       : Navigation_State;
      Show_Files  : Boolean := False;
      Show_Hidden : Boolean := False)
   is
      Search : Search_Type;
      Entry_Info : Directory_Entry_Type;
   begin
      Terminal.Clear_Screen;
      Terminal.Show_Header ("Current: " & To_String (State.Current_Path));
      Put_Line ("Depth: " & State.Current_Depth'Image & " / " & 
                State.Cfg.Max_Depth'Image);
      Put_Line ("");

      begin
         Start_Search (Search, To_String (State.Current_Path), "");
      exception
         when Ada.Directories.Use_Error =>
            Terminal.Error ("Cannot read directory (permission denied)");
            Put_Line ("");
            Put_Line ("========================================");
            return;
      end;

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Entry_Info);

         declare
            Name : constant String := Simple_Name (Entry_Info);
            Full_Path : constant String := Full_Name (Entry_Info);
            Cat : constant File_Types.Category := File_Types.Detect (Full_Path);
         begin
            -- Skip hidden files if not showing them
            if not Show_Hidden and then File_Types.Is_Hidden (Name) then
               goto Continue;
            end if;

            -- Skip files if not showing them
            if not Show_Files and then Cat /= File_Types.Directory then
               goto Continue;
            end if;

            -- Display entry
            Put ("  ");
            Terminal.Put_Colored (
               File_Types.Icon_For (Cat) & " " & Name,
               File_Types.Color_For (Cat)
            );
            New_Line;
         end;

         <<Continue>>
      end loop;

      End_Search (Search);

      Put_Line ("");
      Put_Line ("========================================");
   end List_Contents;

   function Current_Directory (State : Navigation_State) return String is
   begin
      return To_String (State.Current_Path);
   end Current_Directory;

   function Current_Depth (State : Navigation_State) return Natural is
   begin
      return State.Current_Depth;
   end Current_Depth;

   function At_Max_Depth (State : Navigation_State) return Boolean is
   begin
      return State.Current_Depth >= State.Cfg.Max_Depth;
   end At_Max_Depth;

   procedure Navigate_Interactive
     (State : in out Navigation_State;
      BM    : in out Bookmarks.Bookmark_Map)
   is
      Continue : Boolean := True;
   begin
      while Continue loop
         -- Display directory contents
         List_Contents (State, Show_Files => True, Show_Hidden => False);

         -- Check if max depth reached
         if At_Max_Depth (State) then
            Terminal.Success ("Target depth reached!");
            Continue := False;
            exit;
         end if;

         -- Show menu
         Put_Line ("Options:");
         Put_Line ("  [dirname]  - Navigate to directory");
         Put_Line ("  [..]       - Go up one level");
         Put_Line ("  [/]        - Go to root");
         Put_Line ("  [~]        - Go to home");
         Put_Line ("  [b]        - Show bookmarks");
         Put_Line ("  [+]        - Add bookmark");
         Put_Line ("  [q]        - Quit");
         Put_Line ("");

         -- Get user choice
         declare
            Choice : constant String := Terminal.Prompt ("Enter choice");
         begin
            if Choice = "q" or Choice = "Q" then
               Continue := False;

            elsif Choice = ".." then
               Go_Up (State);

            elsif Choice = "/" then
               Change_Directory (State, "/");

            elsif Choice = "~" then
               Go_Home (State);

            elsif Choice = "b" or Choice = "B" then
               Bookmarks.List_All (BM);
               declare
                  BM_Name : constant String := Terminal.Prompt ("Enter bookmark name (or Enter to skip)");
               begin
                  if BM_Name /= "" and then Bookmarks.Exists (BM, BM_Name) then
                     declare
                        BM_Path : constant String := Bookmarks.Get_Path (BM, BM_Name);
                     begin
                        if Ada.Directories.Exists (BM_Path) then
                           Change_Directory (State, BM_Path);
                        else
                           Terminal.Error ("Bookmark path no longer exists");
                        end if;
                     end;
                  end if;
               end;

            elsif Choice = "+" then
               declare
                  BM_Name : constant String := Terminal.Prompt ("Bookmark name");
               begin
                  if BM_Name /= "" then
                     Bookmarks.Add (BM, BM_Name, To_String (State.Current_Path));
                     Terminal.Success ("Bookmark '" & BM_Name & "' added");
                  end if;
               end;

            elsif Choice /= "" then
               -- Try to navigate to the directory
               declare
                  Target : constant String := To_String (State.Current_Path) & "/" & Choice;
               begin
                  if Ada.Directories.Exists (Target) and then 
                     Ada.Directories.Kind (Target) = Directory then
                     Change_Directory (State, Target);
                  else
                     Terminal.Error ("Not a directory: " & Choice);
                     delay 1.0;  -- Brief pause to show error
                  end if;
               exception
                  when Ada.Directories.Name_Error =>
                     Terminal.Error ("Directory not found: " & Choice);
                     delay 1.0;
               end;
            end if;
         end;
      end loop;
   end Navigate_Interactive;

end Navigator;
