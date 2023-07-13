private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;

with Ada.Iterator_Interfaces;

package Tropos is

   type Configuration is tagged private with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Configuration,
     Integer_Literal => To_Integer,
     Real_Literal => To_Real,
     String_Literal => To_String,
     Aggregate => (Empty => Empty, Add_Unnamed => Append);

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Configuration_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Forward_Iterator'Class;

   function To_Integer (Image : String) return Configuration;
   function To_Real (Image : String) return Configuration;
   function To_String (Image : Wide_Wide_String) return Configuration;

   function To_Integer
     (This : Configuration)
      return Integer;

   function To_String
     (This : Configuration)
      return String;

   type Constant_Reference_Type
      (Element : not null access constant Configuration) is private
   with
       Implicit_Dereference => Element;

   function Constant_Reference (This : Configuration;
                                Key  : String)
                                return Constant_Reference_Type;

   function Constant_Reference (This     : Configuration;
                                Position : Cursor)
                                return Constant_Reference_Type;

   function Contains
     (This      : Configuration;
      Child_Tag : String)
      return Boolean;

   function Child
     (This      : Configuration;
      Child_Tag : String)
      return Configuration;

   function Empty return Configuration;
   procedure Append (This : in out Configuration;
                     Item : Configuration);

   procedure Insert (This  : in out Configuration;
                     Tag   : String;
                     Value : Configuration);

   function "+" (S : String) return Configuration;

private

   type Configuration_Access is access constant Configuration;

   package Configuration_Vectors is
      new Ada.Containers.Vectors (Positive, Configuration_Access);

   package Configuration_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Configuration_Access,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   package Attribute_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Constant_Reference_Type
     (Element : not null access constant Configuration) is
      record
         null;
      end record;

   type Configuration is tagged
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Attributes   : Attribute_Maps.Map;
         Child_Vector : Configuration_Vectors.Vector;
         Child_Map    : Configuration_Maps.Map;
      end record;

   type Cursor is
      record
         Container       : Configuration_Access;
         Vector_Position : Configuration_Vectors.Cursor;
         Map_Position    : Configuration_Maps.Cursor;
      end record;

   No_Element : constant Cursor :=
                  (null, Configuration_Vectors.No_Element,
                   Configuration_Maps.No_Element);

   function Has_Element (Position : Cursor) return Boolean
   is (Configuration_Vectors.Has_Element (Position.Vector_Position)
       or else Configuration_Maps.Has_Element (Position.Map_Position));

   type Iterator is
     new Configuration_Iterator_Interfaces.Forward_Iterator with
      record
         Container : Configuration_Access;
      end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end Tropos;