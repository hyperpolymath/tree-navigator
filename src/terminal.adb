-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Terminal utilities implementation
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body Terminal is

   -- ANSI Color codes
   function Color_Code (Color : Color_Type) return String is
   begin
      case Color is
         when Reset   => return ASCII.ESC & "[0m";
         when Red     => return ASCII.ESC & "[31m";
         when Green   => return ASCII.ESC & "[32m";
         when Yellow  => return ASCII.ESC & "[33m";
         when Blue    => return ASCII.ESC & "[34m";
         when Magenta => return ASCII.ESC & "[35m";
         when Cyan    => return ASCII.ESC & "[36m";
         when White   => return ASCII.ESC & "[37m";
         when Bold    => return ASCII.ESC & "[1m";
      end case;
   end Color_Code;

   procedure Put_Colored (Text : String; Color : Color_Type) is
   begin
      Put (Color_Code (Color) & Text & Color_Code (Reset));
   end Put_Colored;

   procedure Put_Line_Colored (Text : String; Color : Color_Type) is
   begin
      Put_Line (Color_Code (Color) & Text & Color_Code (Reset));
   end Put_Line_Colored;

   procedure Clear_Screen is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   procedure Show_Header (Title : String; Width : Positive := 80) is
      Border : constant String := [1 .. Width => '='];
      Padding : constant Natural := Natural'Max (0, (Width - Title'Length) / 2);
   begin
      Put_Line_Colored (Border, Cyan);
      if Padding > 0 then
         Put_Colored ([1 .. Padding => ' '], Cyan);
      end if;
      Put_Line_Colored (Title, Bold);
      Put_Line_Colored (Border, Cyan);
   end Show_Header;

   function Prompt (Message : String; Default : String := "") return String is
   begin
      if Default /= "" then
         Put (Message & " [" & Default & "]: ");
      else
         Put (Message & ": ");
      end if;
      
      declare
         Response : constant String := Ada.Strings.Fixed.Trim (Get_Line, Ada.Strings.Both);
      begin
         if Response = "" and Default /= "" then
            return Default;
         else
            return Response;
         end if;
      end;
   end Prompt;

   function Confirm (Message : String; Default : Boolean := False) return Boolean is
      Default_Str : constant String := (if Default then "Y/n" else "y/N");
      Response : constant String := Prompt (Message & " (" & Default_Str & ")");
      Trimmed : constant String := Ada.Strings.Fixed.Trim (Response, Ada.Strings.Both);
   begin
      if Trimmed = "" then
         return Default;
      end if;
      
      declare
         First_Char : constant Character := Ada.Characters.Handling.To_Lower (Trimmed (Trimmed'First));
      begin
         return First_Char = 'y';
      end;
   end Confirm;

   procedure Success (Message : String) is
   begin
      Put_Line_Colored ("[✓] " & Message, Green);
   end Success;

   procedure Error (Message : String) is
   begin
      Put_Line_Colored ("[✗] " & Message, Red);
   end Error;

   procedure Warning (Message : String) is
   begin
      Put_Line_Colored ("[!] " & Message, Yellow);
   end Warning;

   procedure Info (Message : String) is
   begin
      Put_Line_Colored ("[i] " & Message, Cyan);
   end Info;

   procedure Debug (Message : String; Verbose : Boolean := False) is
   begin
      if Verbose then
         Put_Line_Colored ("[D] " & Message, Blue);
      end if;
   end Debug;

end Terminal;
