pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;

package body Bookmarks is

   procedure Add
     (Map  : in out Bookmark_Map;
      Name : String;
      Path : String)
   is
      BM : constant Bookmark := (
         Name      => new String'(Name),
         Path      => new String'(Path),
         Timestamp => Ada.Calendar.Clock
      );
   begin
      Map.Insert (Name, BM);
   end Add;

   procedure Remove
     (Map  : in out Bookmark_Map;
      Name : String)
   is
   begin
      Map.Delete (Name);
   end Remove;

   function Get_Path
     (Map  : Bookmark_Map;
      Name : String)
      return String
   is
   begin
      return Map.Element (Name).Path.all;
   end Get_Path;

   procedure List_All (Map : Bookmark_Map) is
      use Bookmark_Maps;
   begin
      if Map.Is_Empty then
         Put_Line ("No bookmarks saved.");
         return;
      end if;

      Put_Line ("Bookmarks:");
      for C in Map.Iterate loop
         Put_Line ("  " & Key (C) & " -> " & Element (C).Path.all);
      end loop;
   end List_All;

   procedure Load (Map : out Bookmark_Map; File_Path : String) is
   begin
      Map := Bookmark_Maps.Empty_Map;
      if Ada.Directories.Exists (File_Path) then
         null; -- TODO: Implement file loading
      end if;
   end Load;

   procedure Save (Map : Bookmark_Map; File_Path : String) is
      pragma Unreferenced (Map, File_Path);
   begin
      null; -- TODO: Implement file saving
   end Save;

   function Exists (Map : Bookmark_Map; Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Exists;

end Bookmarks;
