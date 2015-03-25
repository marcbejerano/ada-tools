with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Properties;            use Properties;

procedure Properties_Test is

    function assert_equals(expected, actual: in String) return Boolean is
    begin
        if (expected /= actual) then
            put("Assertion failure. actual=");
            put(actual);
            put(" expected=");
            Put_Line(expected);
            return false;
        else
            return true;
        end if;
    end assert_equals;

    props: Properties.Properties;
    props2: Properties.Properties;
    status: Boolean;
    keys: Key_Vector.Vector;

begin
    Put_Line("*Create");
    props.set_Property("ABC", "987654321");
    props := Create;
    status := assert_equals("", props.Get_Property("ABC"));

    Put_Line("*Create(Properties)");
    props.Set_Property("ABC", "987654321");
    props2 := Create(props);
    props2.Set_Property("Another", "this is a test");
    status := assert_equals("987654321", props2.Get_Property("ABC"));
    status := assert_equals("this is a test", props2.Get_Property("Another"));
    props := Create;

    Put_Line("*Get_Property");
    status := assert_equals("", props.Get_Property("ABC"));

    Put_Line("*Get_Property(String)");
    status := assert_equals("hello world", props.Get_Property("ABC", "hello world"));

    Put_Line("*Set_Property");
    props.Set_Property("ABC", "12345");
    status := assert_equals("12345", props.Get_Property("ABC"));

    Put_Line("*List");
    props.List(Standard_Output);

    Put_Line("*Property_Names");
    keys := props2.Property_Names;
    for key_index in 0 .. Integer(keys.Length) - 1 loop
        Put_Line(To_String(keys(key_index)));
    end loop;

    Put_Line("*String_Property_Names");
    keys := props2.String_Property_Names;
    for key_index in 0 .. Integer(keys.Length) - 1 loop
        Put_Line(To_String(keys(key_index)));
    end loop;

    Put_Line("*Store");
    props.Store(Standard_Output, "This is a comment");

    Put_Line("*Store(Filename)");
    props.Store("test.properties", "This is a comment");

    Put_Line("*Store_To_XML");
    props.Store_To_XML(Standard_Output, "This is a comment");

    Put_Line("*Store_To_XML(Filename)");
    props.Store_To_XML("test.xml", "This is a comment");

    Put_Line("*Load(Filename)");
    props.Load("test2.properties");
    props.List(Standard_Output);
end Properties_Test;
