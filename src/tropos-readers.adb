with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tropos.Readers.Json;

package body Tropos.Readers is

   type Text_File_Stream is
     new Text_Stream with
      record
         File : Ada.Text_IO.File_Type;
      end record;

   overriding function End_Of_Stream (This : Text_File_Stream) return Boolean;
   overriding function Get_Line (This : in out Text_File_Stream) return String;
   overriding procedure Reset (This : in out Text_File_Stream);

   type Text_String_Stream is
     new Text_Stream with
      record
         S    : Ada.Strings.Unbounded.Unbounded_String;
         Done : Boolean := False;
      end record;

   overriding function End_Of_Stream
     (This : Text_String_Stream)
      return Boolean;

   overriding function Get_Line
     (This : in out Text_String_Stream)
      return String;

   overriding procedure Reset
     (This : in out Text_String_Stream);

   ------------
   -- Create --
   ------------

   function Create
     (Source : in out Text_Stream'Class)
      return Reader'Class
   is
   begin
      if Tropos.Readers.Json.Try (Source) then
         Source.Reset;
         return Tropos.Readers.Json.Read;
      end if;
      raise Constraint_Error with
        "no readers found";
   end Create;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding function End_Of_Stream
     (This : Text_File_Stream)
      return Boolean
   is
   begin
      return Ada.Text_IO.End_Of_File (This.File);
   end End_Of_Stream;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding function End_Of_Stream
     (This : Text_String_Stream)
      return Boolean
   is
   begin
      return This.Done;
   end End_Of_Stream;

   -----------------
   -- File_Stream --
   -----------------

   function File_Stream
     (Path : String)
      return Text_Stream'Class
   is
   begin
      return This : Text_File_Stream do
         Ada.Text_IO.Open (This.File, Ada.Text_IO.In_File, Path);
      end return;
   end File_Stream;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (This : in out Text_File_Stream)
      return String
   is
   begin
      return Ada.Text_IO.Get_Line (This.File);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (This : in out Text_String_Stream)
      return String
   is
   begin
      if This.Done then
         raise Ada.Text_IO.End_Error;
      end if;
      return Ada.Strings.Unbounded.To_String (This.S);
   end Get_Line;

   ----------
   -- Read --
   ----------

   function Read
     (Source : in out Text_Stream'Class)
      return Configuration
   is
      R : constant Reader'Class := Create (Source);
   begin
      return R.Read (Source);
   end Read;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (This : in out Text_File_Stream) is
   begin
      Ada.Text_IO.Reset (This.File);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (This : in out Text_String_Stream)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      This.Done := This.S /= "";
   end Reset;

   -------------------
   -- String_Stream --
   -------------------

   function String_Stream
     (Text : String)
      return Text_Stream'Class
   is
   begin
      return This : Text_String_Stream do
         This.S := Ada.Strings.Unbounded.To_Unbounded_String (Text);
         This.Done := Text = "";
      end return;
   end String_Stream;

end Tropos.Readers;
