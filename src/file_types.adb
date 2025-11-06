pragma Ada_2022;
with Ada.Strings.Fixed;
with Ada.Directories;

package body File_Types is

   function Detect (Path : String) return Category is
      use Ada.Directories;
      Kind : constant File_Kind := Ada.Directories.Kind (Path);
   begin
      if Kind = Ada.Directories.Directory then
         return File_Types.Directory;
      end if;

      -- Check file extensions
      declare
         Name : constant String := Simple_Name (Path);
      begin
         -- Check if hidden
         if Is_Hidden (Name) then
            return Hidden_File;
         end if;

         -- Check file extensions
         if Has_Extension (Name, ".sh") or Has_Extension (Name, ".bash") then
            return Shell_Script;
         elsif Has_Extension (Name, ".conf") or Has_Extension (Name, ".cfg") then
            return System_Config;
         elsif Has_Extension (Name, ".json") or Has_Extension (Name, ".xml") or
               Has_Extension (Name, ".yaml") or Has_Extension (Name, ".yml") then
            return Data_File;
         elsif Has_Extension (Name, ".exe") or Has_Extension (Name, ".bin") then
            return Executable;
         elsif Kind = Ordinary_File then
            return Regular_File;
         else
            return Regular_File;
         end if;
      end;
   end Detect;

   function Icon_For (Cat : Category) return String is
   begin
      case Cat is
         when File_Types.Directory => return "📁";
         when Executable           => return "⚙️";
         when System_Config        => return "⚙️";
         when Shell_Script         => return "📜";
         when Data_File            => return "📄";
         when Hidden_File          => return "👻";
         when Regular_File         => return "📄";
      end case;
   end Icon_For;

   function Color_For (Cat : Category) return Terminal.Color_Type is
   begin
      case Cat is
         when File_Types.Directory => return Terminal.Cyan;
         when Executable           => return Terminal.Green;
         when System_Config        => return Terminal.Yellow;
         when Shell_Script         => return Terminal.Green;
         when Data_File            => return Terminal.White;
         when Hidden_File          => return Terminal.Yellow;
         when Regular_File         => return Terminal.White;
      end case;
   end Color_For;

   function Is_Hidden (Name : String) return Boolean is
   begin
      return Name'Length > 0 and then Name (Name'First) = '.';
   end Is_Hidden;

   function Has_Extension (Path : String; Ext : String) return Boolean is
      use Ada.Strings.Fixed;
      Ext_Pos : constant Natural := Index (Path, Ext, Going => Ada.Strings.Backward);
   begin
      return Ext_Pos > 0 and then Ext_Pos = Path'Last - Ext'Length + 1;
   end Has_Extension;

end File_Types;
