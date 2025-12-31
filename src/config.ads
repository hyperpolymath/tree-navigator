-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Configuration management with strong typing
pragma Ada_2022;

package Config is

   type String_Access is access all String;

   -- Configuration record
   type Configuration is record
      Max_Depth     : Positive := 10;
      Show_Hidden   : Boolean  := False;
      Color_Mode    : Boolean  := True;
      Cache_Enabled : Boolean  := True;
      Verbose       : Boolean  := False;
      Safe_Mode     : Boolean  := True;
   end record;

   -- File paths
   type File_Paths is record
      Config_Dir    : String_Access;
      Cache_Dir     : String_Access;
      Bookmark_File : String_Access;
      History_File  : String_Access;
   end record;

   -- Load configuration from file
   procedure Load (Config : out Configuration; Paths : File_Paths);

   -- Save configuration to file
   procedure Save (Config : Configuration; Paths : File_Paths);

   -- Get default configuration
   function Default return Configuration;

   -- Validate configuration
   function Is_Valid (Config : Configuration) return Boolean
     with Post => (if Is_Valid'Result then Config.Max_Depth in 1 .. 100);

end Config;
