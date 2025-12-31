-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Interactive directory navigation
pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;
with Bookmarks;

package Navigator is

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);

   type Navigation_State is record
      Cfg              : Config.Configuration;
      Current_Path     : Unbounded_String;
      Navigation_Stack : String_Vectors.Vector;
      Current_Depth    : Natural;
   end record;

   function Initialize (Cfg : Config.Configuration) return Navigation_State;

   procedure Change_Directory
     (State : in out Navigation_State;
      Path  : String);

   procedure Go_Up (State : in out Navigation_State);

   procedure Go_Home (State : in out Navigation_State);

   procedure List_Contents
     (State       : Navigation_State;
      Show_Files  : Boolean := False;
      Show_Hidden : Boolean := False);

   function Current_Directory (State : Navigation_State) return String;

   function Current_Depth (State : Navigation_State) return Natural;

   function At_Max_Depth (State : Navigation_State) return Boolean;

   procedure Navigate_Interactive
     (State : in out Navigation_State;
      BM    : in out Bookmarks.Bookmark_Map);

end Navigator;
