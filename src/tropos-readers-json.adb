with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Tropos.Readers.Json is

   type Json_Reader is new Reader with
      record
         null;
      end record;

   overriding function Read
     (This   : Json_Reader;
      Source : in out Text_Stream'Class)
      return Configuration;

   function Parse
     (Stream  : in out Text_Stream'Class)
      return Configuration;

   -----------
   -- Parse --
   -----------

   function Parse
     (Stream  : in out Text_Stream'Class)
      return Configuration
   is
      Current_Line       : Ada.Strings.Unbounded.Unbounded_String;
      Current_Line_Index : Natural := 0;
      Current_Last       : Natural := 0;
      Current_Index      : Natural := 0;
      Current_Character  : Character := ' ';
      Done               : Boolean := False;

      procedure Start;
      procedure Finish;

      procedure Next_Character;

      procedure Skip_Whitespace;

      procedure Error (Message : String);

      function Parse_Json_Element return Configuration;
      function Parse_Json_Object return Configuration;
      function Parse_Json_Array return Configuration;

      function Parse_Terminal return String;
      function Parse_Rest_Of_String return String;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Strings.Unbounded.To_String (Current_Line));

         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Stream.Name
            & ":" & Current_Line_Index'Image
            & ": " & Message);

         raise Constraint_Error with Message;
      end Error;

      ------------
      -- Finish --
      ------------

      procedure Finish is
      begin
         Skip_Whitespace;
         if not Done then
            Error ("extra ignored");
         end if;
      end Finish;

      --------------------
      -- Next_Character --
      --------------------

      procedure Next_Character is
      begin
         if Current_Index >= Current_Last then
            begin
               Current_Line_Index := Current_Line_Index + 1;
               if Stream.End_Of_Stream then
                  Done := True;
                  return;
               end if;

               Current_Line :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Stream.Get_Line);
               Current_Last :=
                 Ada.Strings.Unbounded.Length (Current_Line);

            end;
            Current_Index := 0;
            Next_Character;
            return;
         end if;

         Current_Index := Current_Index + 1;
         Current_Character :=
           Ada.Strings.Unbounded.Element (Current_Line, Current_Index);
      end Next_Character;

      ----------------------
      -- Parse_Json_Array --
      ----------------------

      function Parse_Json_Array return Configuration is
         This : Configuration;
      begin
         while not Done
           and then Current_Character /= ']'
         loop
            This.Append (Parse_Json_Element);
            Skip_Whitespace;

            if Current_Character = ',' then
               Next_Character;
               Skip_Whitespace;
            elsif Current_Character /= ']' then
               Error ("missing ',' in array");
            end if;

         end loop;

         if Current_Character = ']' then
            Next_Character;
         else
            Error ("missing ']'");
         end if;

         return This;

      end Parse_Json_Array;

      ------------------------
      -- Parse_Json_Element --
      ------------------------

      function Parse_Json_Element return Configuration is
      begin
         Skip_Whitespace;

         if Done then
            return Empty;
         end if;

         case Current_Character is
            when '{' =>
               Next_Character;
               Skip_Whitespace;
               return Parse_Json_Object;
            when '[' =>
               Next_Character;
               Skip_Whitespace;
               return Parse_Json_Array;
            when others =>
               declare
                  Id : constant String := Parse_Terminal;
               begin
                  return +Id;
               end;
         end case;
      end Parse_Json_Element;

      -----------------------
      -- Parse_Json_Object --
      -----------------------

      function Parse_Json_Object return Configuration is
         This : Configuration;
      begin
         while not Done
           and then Current_Character /= '}'
         loop
            case Current_Character is
               when '"' =>
                  declare
                     Id : constant String := Parse_Terminal;
                  begin
                     Skip_Whitespace;
                     if Current_Character = ':' then
                        Next_Character;
                     else
                        Error ("missing value");
                     end if;

                     This.Insert (Id, Parse_Json_Element);
                  end;
               when others =>
                  Error ("missing field name");
            end case;

            Skip_Whitespace;

            if Current_Character = ',' then
               Next_Character;
            elsif Current_Character = '}' then
               null;
            else
               Error ("missing ','");
            end if;

            Skip_Whitespace;

         end loop;

         if Current_Character = '}' then
            Next_Character;
         else
            Error ("missing close brace");
         end if;

         return This;

      end Parse_Json_Object;

      --------------------------
      -- Parse_Rest_Of_String --
      --------------------------

      function Parse_Rest_Of_String return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;

         procedure Save;

         ----------
         -- Save --
         ----------

         procedure Save is
         begin
            Append (Result, Current_Character);
            Next_Character;
         end Save;

      begin
         while not Done and then Current_Character /= '"' loop
            if Current_Character = '\' then
               Next_Character;
            end if;
            Save;
         end loop;

         if Done then
            Error ("missing close quote");
         end if;

         Next_Character;
         return To_String (Result);
      end Parse_Rest_Of_String;

      --------------------
      -- Parse_Terminal --
      --------------------

      function Parse_Terminal return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;

         procedure Save;

         ----------
         -- Save --
         ----------

         procedure Save is
         begin
            Append (Result, Current_Character);
            Next_Character;
         end Save;

      begin
         if Current_Character = '"' then
            Next_Character;
            return Parse_Rest_Of_String;
         elsif Current_Character in '0' .. '9' | '+' | '-' then
            if Current_Character in '-' | '+' then
               Save;
            end if;

            while not Done
              and then Current_Character in
                '0' .. '9' | '.' | 'e' | 'E' | '+' | '-'
            loop
               Save;
            end loop;

         elsif Ada.Characters.Handling.Is_Letter (Current_Character) then
            while not Done
              and then Ada.Characters.Handling.Is_Alphanumeric
                (Current_Character)
            loop
               Save;
            end loop;
         else
            Error ("expected a name");
            return "";
         end if;

         return To_String (Result);

      end Parse_Terminal;

      ---------------------
      -- Skip_Whitespace --
      ---------------------

      procedure Skip_Whitespace is
      begin
         while not Done
           and then (Ada.Characters.Handling.Is_Space (Current_Character)
                     or else Current_Character = Character'Val (9))
         loop
            Next_Character;
         end loop;
      end Skip_Whitespace;

      -----------
      -- Start --
      -----------

      procedure Start is
      begin
         Stream.Reset;
         Next_Character;
      end Start;

   begin
      Start;
      return Result : constant Configuration := Parse_Json_Element do
         Finish;
      end return;
   end Parse;

   ----------
   -- Read --
   ----------

   function Read return Reader'Class is
   begin
      return This : constant Json_Reader := (null record);
   end Read;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This   : Json_Reader;
      Source : in out Text_Stream'Class)
      return Configuration
   is
   begin
      return Parse (Source);
   end Read;

   ---------
   -- Try --
   ---------

   function Try (Source : in out Text_Stream'Class) return Boolean is
   begin
      declare
         Element : constant Configuration := Read (Json_Reader'(null record),
                                                   Source)
           with Unreferenced;
      begin
         return True;
      exception
         when others =>
            return False;
      end;
   end Try;

end Tropos.Readers.Json;
