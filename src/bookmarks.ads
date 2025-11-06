-- Bookmark management with persistence
pragma Ada_2022;
with Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;

package Bookmarks is

   type String_Access is access all String;

   -- Bookmark record
   type Bookmark is record
      Name      : String_Access;
      Path      : String_Access;
      Timestamp : Ada.Calendar.Time;
   end record;

   -- Bookmark storage
   package Bookmark_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Bookmark);

   subtype Bookmark_Map is Bookmark_Maps.Map;

   -- Add a bookmark
   procedure Add
     (Map  : in out Bookmark_Map;
      Name : String;
      Path : String)
     with Pre => Name'Length > 0 and Path'Length > 0;

   -- Remove a bookmark
   procedure Remove
     (Map  : in out Bookmark_Map;
      Name : String)
     with Pre => Map.Contains (Name);

   -- Get bookmark path
   function Get_Path
     (Map  : Bookmark_Map;
      Name : String)
      return String
     with Pre => Map.Contains (Name);

   -- List all bookmarks
   procedure List_All (Map : Bookmark_Map);

   -- Load bookmarks from file
   procedure Load (Map : out Bookmark_Map; File_Path : String);

   -- Save bookmarks to file
   procedure Save (Map : Bookmark_Map; File_Path : String);

   -- Check if bookmark exists
   function Exists (Map : Bookmark_Map; Name : String) return Boolean;

end Bookmarks;
