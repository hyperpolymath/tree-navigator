-- File type detection and classification
pragma Ada_2022;
with Ada.Directories;
with Terminal;

package File_Types is

   -- File category
   type Category is (
      Directory,
      Executable,
      System_Config,
      Shell_Script,
      Data_File,
      Hidden_File,
      Regular_File
   );

   -- Detect file category
   function Detect (Path : String) return Category
     with Pre => Ada.Directories.Exists (Path);

   -- Get color for category
   function Color_For (Cat : Category) return Terminal.Color_Type;

   -- Get icon for category
   function Icon_For (Cat : Category) return String;

   -- Check if file matches extension
   function Has_Extension (Path : String; Ext : String) return Boolean;

   -- Check if file is hidden (starts with dot)
   function Is_Hidden (Name : String) return Boolean;

end File_Types;