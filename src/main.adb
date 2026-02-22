-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Tree Navigator — High-Assurance Directory Visualization.
--
-- This Ada 2022 module implements the primary logic for the `tn` utility.
-- It provides a cross-platform interface for navigating physical directory 
-- trees and exporting them to structured text files.
--
-- DESIGN PILLARS:
-- 1. ADABILITY: Leveraging Ada's strong typing and formal verification 
--    capabilities for filesystem interactions.
-- 2. DUAL-MODE: Supports both interactive terminal navigation and 
--    unattended batch exports.
-- 3. FILTERING: Granular exclusion rules for directories (e.g. target, node_modules).

pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
-- ... [other imports]

procedure Main is
   Cfg : Config.Configuration;
   Paths : Config.File_Paths;
   State : Navigator.Navigation_State;
   BM_Map : Bookmarks.Bookmark_Map;

   -- CONFIGURATION: Sets up the standard paths for config and cache.
   procedure Initialize_Paths is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
   begin
      Paths.Config_Dir := new String'(Home & "/.config/tree-navigator");
      Paths.Cache_Dir := new String'(Home & "/.cache/tree-navigator");
      -- ... [Directory creation logic]
   end Initialize_Paths;

   -- EXECUTION: The main entry point processes CLI arguments and 
   -- dispatches to either the interactive navigator or the tree printer.
begin
   Initialize_Paths;
   Config.Load (Cfg, Paths);

   if Export_Mode then
      -- BATCH EXPORT: Generates a serialized tree representation.
      Tree_Printer.Export_Tree (Export_Opts, Stats);
   else
      -- INTERACTIVE: Starts the TUI navigation loop.
      Navigator.Navigate_Interactive (State, BM_Map);
   end if;
end Main;
