pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body Terminal is

   function Color_Code (Color : Color_Type) return String is
   begin
      case Color is
         when Reset   => return Character'Val (16#1B#) & "[0m";
         when Red     => return Character'Val (16#1B#) & "[0;31m";
         when Green   => return Character'Val (16#1B#) & "[0;32m";
         when Yellow  => return Character'Val (16#1B#) & "[1;33m";
         when Blue    => return Character'Val (16#1B#) & "[0;34m";
         when Magenta => return Character'Val (16#1B#) & "[0;35m";
         when Cyan    => return Character'Val (16#1B#) & "[0;36m";
         when White   => return Character'Val (16#1B#) & "[1;37m";
         when Bold    => return Character'Val (16#1B#) & "[1m";
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

   procedure Success (Message : String) is
   begin
      Put_Colored ("[✓] ", Green);
      Put_Line (Message);
   end Success;

   procedure Error (Message : String) is
   begin
      Put_Colored ("[✗] ", Red);
      Put_Line (Message);
   end Error;

   procedure Warning (Message : String) is
   begin
      Put_Colored ("[!] ", Yellow);
      Put_Line (Message);
   end Warning;

   procedure Info (Message : String) is
   begin
      Put_Colored ("[i] ", Blue);
      Put_Line (Message);
   end Info;

   procedure Debug (Message : String; Verbose : Boolean := False) is
   begin
      if Verbose then
         Put_Colored ("[D] ", Cyan);
         Put_Line (Message);
      end if;
   end Debug;

   function Prompt (Message : String; Default : String := "") return String is
      Input : String (1 .. 256);
      Last  : Natural;
   begin
      Put (Message);
      if Default /= "" then
         Put (" [" & Default & "]");
      end if;
      Put (": ");

      Get_Line (Input, Last);

      if Last = 0 and Default /= "" then
         return Default;
      elsif Last > 0 then
         return Input (1 .. Last);
      else
         return "";
      end if;
   end Prompt;

   function Confirm (Message : String; Default : Boolean := False) return Boolean is
      Response : constant String := Prompt (Message & " (y/n)", 
                                           (if Default then "y" else "n"));
   begin
      return Response'Length > 0 and then (Response (1) = 'y' or Response (1) = 'Y');
   end Confirm;

   procedure Clear_Screen is
   begin
      Put (Character'Val (16#1B#) & "[2J");  -- Clear screen
      Put (Character'Val (16#1B#) & "[H");   -- Move cursor to home
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

end Terminal;