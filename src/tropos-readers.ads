package Tropos.Readers is

   type Text_Stream is abstract tagged limited private;

   function End_Of_Stream (This : Text_Stream) return Boolean is abstract;
   function Get_Line (This : in out Text_Stream) return String is abstract;
   procedure Reset (This : in out Text_Stream) is abstract;

   function File_Stream
     (Path : String)
      return Text_Stream'Class;

   function String_Stream
     (Text : String)
      return Text_Stream'Class;

   type Reader is abstract tagged private;

   function Read
     (This   : Reader;
      Source : in out Text_Stream'Class)
      return Configuration
      is abstract;

   function Create
     (Source : in out Text_Stream'Class)
      return Reader'Class;

   function Read
     (Source : in out Text_Stream'Class)
      return Configuration;

private

   type Text_Stream is abstract tagged limited
      record
         null;
      end record;

   type Reader is abstract tagged
      record
         null;
      end record;

end Tropos.Readers;
