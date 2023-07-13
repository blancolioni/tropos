with Ada.Text_IO;
with Tropos.Readers;

package body Tropos_Tests is

   procedure Glossary_Test;
   procedure Widget_Test;

   -------------------
   -- Glossary_Test --
   -------------------

   procedure Glossary_Test is
      Stream : Tropos.Readers.Text_Stream'Class :=
                 Tropos.Readers.File_Stream
                   ("./glossary.json");
      Config : constant Tropos.Configuration :=
                 Tropos.Readers.Read (Stream);
      Glossary : constant Tropos.Configuration :=
                   Config ("glossary");
   begin
      Ada.Text_IO.Put_Line
        (Glossary ("title").To_String);
   end Glossary_Test;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Glossary_Test;
      Widget_Test;
   end Run_Tests;

   -----------------
   -- Widget_Test --
   -----------------

   procedure Widget_Test is
      Stream : Tropos.Readers.Text_Stream'Class :=
                 Tropos.Readers.File_Stream
                   ("./widget.json");
      Config : constant Tropos.Configuration :=
                 Tropos.Readers.Read (Stream);
      Widgets : constant Tropos.Configuration :=
                  Config ("widget");
   begin
      for W of Widgets loop
         if W.Contains ("title") then
            Ada.Text_IO.Put_Line (W ("title").To_String);
         elsif W.Contains ("name") then
            Ada.Text_IO.Put_Line (W ("name").To_String);
         end if;
      end loop;
   end Widget_Test;

end Tropos_Tests;
