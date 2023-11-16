with Ada.Containers.Indefinite_Vectors;

package body Tropos.Readers.CSV is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Split (S  : String;
                   Ch : Character)
                   return String_Vectors.Vector;

   Possible_Separators : constant String :=
                           ",;|:" & Character'Val (9);

   type CSV_Reader is new Reader with
      record
         Header    : Boolean := True;
         Separator : Character := ';';
         Delimiter : Character := '"';
      end record;

   overriding function Read
     (This   : CSV_Reader;
      Source : in out Text_Stream'Class)
      return Configuration;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This   : CSV_Reader;
      Source : in out Text_Stream'Class)
      return Configuration
   is
      Header : String_Vectors.Vector;
      Result : Tropos.Configuration;
   begin
      if This.Header then
         Header := Split (Source.Get_Line, This.Separator);
      end if;
      while not Source.End_Of_Stream loop
         declare
            Line : constant String_Vectors.Vector :=
                     Split (Source.Get_Line, This.Separator);
            Config : Tropos.Configuration;
         begin
            for I in 1 .. Line.Last_Index loop
               Config.Insert
                 (Tag => (if I <= Header.Last_Index
                          then Header (I)
                          else ""),
                  Value => +Line.Element (I));
            end loop;
            Result.Append (Config);
         end;
      end loop;
      return Result;
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Header    : Boolean := True;
      Separator : Character := ';';
      Delimiter : Character := '"')
      return Reader'Class
   is
   begin
      return CSV_Reader'
        (Header => Header, Separator => Separator, Delimiter => Delimiter);
   end Read;

   -----------
   -- Split --
   -----------

   function Split (S  : String;
                   Ch : Character)
                   return String_Vectors.Vector
   is
      Start : Positive := S'First;
   begin
      return Result : String_Vectors.Vector do
         for I in S'Range loop
            if S (I) = Ch then
               Result.Append (S (Start .. I - 1));
               Start := I + 1;
            end if;
         end loop;
         Result.Append (S (Start .. S'Last));
      end return;
   end Split;

   ---------
   -- Try --
   ---------

   function Try (Source : in out Text_Stream'Class) return Boolean is
   begin
      for Ch of Possible_Separators loop
         Source.Reset;
         declare
            OK    : Boolean := True;
            Count : constant Natural := Split (Source.Get_Line, Ch).Last_Index;
         begin
            if Count > 0 then
               for I in 1 .. 3 loop
                  declare
                     Field_Count : constant natural :=
                                     Split (Source.Get_Line, Ch).Last_Index;
                  begin
                     OK := Field_Count in Count - 1 .. Count + 1;
                  end;
               end loop;
               if OK then
                  return True;
               end if;
            end if;
         end;
      end loop;
      return False;
   end Try;

end Tropos.Readers.CSV;
