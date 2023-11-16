package Tropos.Readers.CSV is

   function Try (Source : in out Text_Stream'Class) return Boolean;

   function Read
     (Header    : Boolean := True;
      Separator : Character := ';';
      Delimiter : Character := '"')
     return Reader'Class;

end Tropos.Readers.CSV;
