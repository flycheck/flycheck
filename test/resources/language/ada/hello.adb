with Ada.Text_IO;
with Ada.Command_Line;

procedure Hello is
   package IO renames Ada.Text_IO;
   Name : String :=  Ada.Command_Line.Argument (1);
begin
   pragma Foo;
   IO.Put_Line("Hello, world!");
   IO.New_Line;
end Hello;
