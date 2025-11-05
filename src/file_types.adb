pragma Ada_2022;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed;
with Terminal;

package body File_Types is

   function Detect (Path : String) return Category is
      Kind : constant File_Kind := Ada.Directories.Kind (Path);
   begin
      -- Directory check
      if Kind = Directory then
         return Directory;
      end if;

      -- Get filename
      declare
         Name : constant String := Simple_Name (Path);
      begin
         -- Hidden file check
         if Is_Hidden (Name) then
            return Hidden_File;
         end if;

         -- Executable check (on Unix systems)
         if Kind = Ordinary_File then
            declare
               use Ada.Directories;
            begin
               -- This is simplified; real implementation would check permissions
               if Has_Extension (Name, ".sh") or Has_Extension (Name, ".bash") then
                  return Shell_Script;
               elsif Has_Extension (Name, ".conf") or Has_Extension (Name, ".cfg") 
                  or Has_Extension (Name, ".ini") or Has_Extension (Name, ".service") then
                  return System_Config;
               elsif Has_Extension (Name, ".json") or Has_Extension (Name, ".xml") 
                  or Has_Extension (Name, ".csv") or Has_Extension (Name, ".db") then
                  return Data_File;
               end if;
            end;
         end if;
      end;

      return Regular_File;
   end Detect;

   function Color_For (Cat : Category) return Terminal.Color_Type is
   begin
      case Cat is
         when Directory      => return Terminal.Blue;
         when Executable     => return Terminal.Green;
         when System_Config  => return Terminal.Yellow;
         when Shell_Script   => return Terminal.Cyan;
         when Data_File      => return Terminal.Magenta;
         when Hidden_File    => return Terminal.Red;
         when Regular_File   => return Terminal.White;
      end case;
   end Color_For;

   function Icon_For (Cat : Category) return String is
   begin
      case Cat is
         when Directory      => return "📁";
         when Executable     => return "⚙️";
         when System_Config  => return "🔧";
         when Shell_Script   => return "📜";
         when Data_File      => return "📊";
         when Hidden_File    => return "👁️";
         when Regular_File   => return "📄";
      end case;
   end Icon_For;

   function Has_Extension (Path : String; Ext : String) return Boolean is
      use Ada.Strings.Fixed;
      Ext_Pos : constant Natural := Index (Path, Ext, Going => Backward);
   begin
      return Ext_Pos > 0 and then Ext_Pos = Path'Last - Ext'Length + 1;
   end Has_Extension;

   function Is_Hidden (Name : String) return Boolean is
   begin
      return Name'Length > 0 and then Name (Name'First) = '.';
   end Is_Hidden;

end File_Types;