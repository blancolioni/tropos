package Tropos.Readers.Json is

   function Try (Source : in out Text_Stream'Class) return Boolean;

   function Read return Reader'Class;

end Tropos.Readers.Json;
