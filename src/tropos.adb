with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Tropos is

   ---------
   -- "+" --
   ---------

   function "+" (S : String) return Configuration is
   begin
      return Configuration'
        (Name       => Ada.Strings.Unbounded.To_Unbounded_String (S),
         others     => <>);
   end "+";

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Configuration;
                     Item : Configuration)
   is
      Child : constant Configuration_Access := new Configuration'(Item);
   begin
      This.Child_Vector.Append (Child);
   end Append;

   -----------
   -- Child --
   -----------

   function Child
     (This      : Configuration;
      Child_Tag : String)
      return Configuration
   is
   begin
      if This.Contains (Child_Tag) then
         return This (Child_Tag);
      else
        return Empty;
      end if;
   end Child;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference (This : Configuration;
                                Key  : String)
                                return Constant_Reference_Type
   is
      pragma Assert (This.Child_Map.Contains (Key));
      Child : constant Configuration_Access := This.Child_Map (Key);
   begin
      return Constant_Reference_Type'(Element => Child);
   end Constant_Reference;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference (This     : Configuration;
                                Position : Cursor)
                                return Constant_Reference_Type
   is
      Child : Configuration_Access;
   begin
      if Configuration_Vectors.Has_Element (Position.Vector_Position) then
         Child := Configuration_Vectors.Element (Position.Vector_Position);
      elsif Configuration_Maps.Has_Element (Position.Map_Position) then
         Child := Configuration_Maps.Element (Position.Map_Position);
      else
         raise Constraint_Error with "position has no element";
      end if;
      return Constant_Reference_Type'(Element => Child);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (This      : Configuration;
      Child_Tag : String)
      return Boolean
   is
   begin
      return This.Child_Map.Contains (Child_Tag);
   end Contains;

   -----------
   -- Empty --
   -----------

   function Empty return Configuration is
   begin
      return (others => <>);
   end Empty;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
   begin
      if not Object.Container.Child_Vector.Is_Empty then
         return (Object.Container, Object.Container.Child_Vector.First,
                 Configuration_Maps.No_Element);
      elsif not Object.Container.Child_Map.Is_Empty then
         return (Object.Container, Configuration_Vectors.No_Element,
                 Object.Container.Child_Map.First);
      else
         return No_Element;
      end if;
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Iterator;
      Position : Cursor)
      return Cursor
   is
   begin
      if Configuration_Vectors.Has_Element (Position.Vector_Position) then
         return (Position with delta
                   Vector_Position =>
                     Configuration_Vectors.Next (Position.Vector_Position));
      elsif Configuration_Maps.Has_Element (Position.Map_Position) then
         return (Position with delta
                   Map_Position =>
                     Configuration_Maps.Next (Position.Map_Position));
      else
         return No_Element;
      end if;
   end Next;

   ------------
   -- Insert --
   ------------

   procedure Insert (This  : in out Configuration;
                     Tag   : String;
                     Value : Configuration)
   is
      Child : constant Configuration_Access := new Configuration'(Value);
   begin
      This.Child_Map.Insert (Tag, Child);
   end Insert;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      return It : constant Iterator :=
        Iterator'
          (Container => Container'Unchecked_Access);
   end Iterate;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Image : String) return Configuration is
   begin
      return To_String (Integer'Wide_Wide_Image (Integer'Value (Image)));
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (This : Configuration)
      return Integer
   is
   begin
      return Integer'Value
        (Ada.Strings.Unbounded.To_String
           (This.Name));
   end To_Integer;

   -------------
   -- To_Real --
   -------------

   function To_Real (Image : String) return Configuration is
   begin
      return To_String (Long_Float'Wide_Wide_Image (Long_Float'Value (Image)));
   end To_Real;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (This : Configuration)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (This.Name);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Image : Wide_Wide_String) return Configuration is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      Img : constant String := Encode (Image);
   begin
      return Configuration'
        (Name       => Ada.Strings.Unbounded.To_Unbounded_String (Img),
         others     => <>);
   end To_String;

end Tropos;
