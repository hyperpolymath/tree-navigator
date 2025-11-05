-- Terminal utilities for colored output and user interaction
pragma Ada_2022;

package Terminal is

   -- Color codes (ANSI escape sequences)
   type Color_Type is (
      Reset,
      Red,
      Green,
      Yellow,
      Blue,
      Magenta,
      Cyan,
      White,
      Bold
   );

   -- Get ANSI code for color
   function Color_Code (Color : Color_Type) return String;

   -- Print colored text
   procedure Put_Colored (Text : String; Color : Color_Type);
   procedure Put_Line_Colored (Text : String; Color : Color_Type);

   -- Status messages
   procedure Success (Message : String);
   procedure Error (Message : String);
   procedure Warning (Message : String);
   procedure Info (Message : String);
   procedure Debug (Message : String; Verbose : Boolean := False);

   -- User input
   function Prompt (Message : String; Default : String := "") return String;
   function Confirm (Message : String; Default : Boolean := False) return Boolean;

   -- Clear screen
   procedure Clear_Screen;

   -- Display header
   procedure Show_Header (Title : String; Width : Positive := 80);

end Terminal;