-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Configuration management implementation
pragma Ada_2022;

package body Config is

   procedure Load (Config : out Configuration; Paths : File_Paths) is
      pragma Unreferenced (Paths);
   begin
      Config := Default;
   end Load;

   procedure Save (Config : Configuration; Paths : File_Paths) is
      pragma Unreferenced (Config, Paths);
   begin
      null; -- TODO: Implement file saving
   end Save;

   function Default return Configuration is
   begin
      return Configuration'(
         Max_Depth     => 10,
         Show_Hidden   => False,
         Color_Mode    => True,
         Cache_Enabled => True,
         Verbose       => False,
         Safe_Mode     => True
      );
   end Default;

   function Is_Valid (Config : Configuration) return Boolean is
   begin
      return Config.Max_Depth in 1 .. 100;
   end Is_Valid;

end Config;
