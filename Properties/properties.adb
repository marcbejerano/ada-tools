-- @(#)File:            properties.adb
-- @(#)Last changed:    Mar 18 2015 10:30:00
-- @(#)Purpose:         Java properties file support
-- @(#)Author:          Marc Bejerano <marcbejerano@gmail.com>
-- @(#)Copyright:       
-- @(#)Product:         

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Containers.Hashed_Maps;    use Ada.Containers;
with Ada.Strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNAT.Calendar.Time_IO;         use GNAT.Calendar.Time_IO;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;      use Ada.Text_IO.Unbounded_IO;

package body Properties is

    use type Hash_Table.Cursor;

    --
    -- Escape all of the characters that may be interpreted as tokens
    -- by the loader with a backslash (\) character. For the keys,
    -- all spaces inside the key name are escaped. For keys and values,
    -- all characters in the set [# ! = :] are also prefixed. For 
    -- values, only the leading whitespace is escaped.
    -- @param str String to escape
    -- @param is_key_string Flag indicating this is a key and not a value
    -- @return Escaped string
    --
    function Escape(str: in Unbounded_String; is_key_string: in Boolean := true) return Unbounded_String is
        result : Unbounded_String;
        ch : Character;
        is_leading : Boolean := true;
    begin
        for idx in 1 .. Length(str) loop
            ch := Element(str, idx);
            if ch = '#' or ch = '!' or ch = ' ' or ch = '\' or ch = ':' or ch = '=' then
                if is_key_string or (not is_key_string and ch = ' ' and is_leading) then
                    Append(result, '\');
                end if;
            end if;
            if is_leading and ch /= ' ' then
                is_leading := false;
            end if;
            Append(result, ch);
        end loop;
        return result;
    end Escape;

    --
    -- Hash the given key into a Hash object.
    -- @param key Key to hash
    -- @return Hash object
    --
    function Key_Hashed(key: in Unbounded_String) return Hash_Type is
    begin
        return Hash(key);
    end Key_Hashed;

    --
    -- Properties tagged record Copy function. This function will take into
    -- account the added data member containing the default values.
    -- @param Source Properties object
    -- @param Capacity
    function Copy(Source: Properties; Capacity: Count_Type := 0) return Properties is
        props: Properties;
    begin
        props := Source;
        return props;
    end Copy;

    --
    -- Create an empty Properties object.
    -- @return Properties object
    --
    function Create return Properties is
        props: Properties;
    begin
        return props;
    end Create;

    --
    -- Create a Properties object with initial data provided by the
    -- given default container.
    -- @param defaults Default container of property data
    -- @return Properties object
    --
    function Create(defaults: in Properties) return Properties is
        props: Properties;
    begin
        props.defaults := Hash_Table.Map(defaults);
        return props;
    end Create;

    --
    -- Get the named property from the properties container. If the requested
    -- key does not exist in the container then an empty string will be returned.
    -- @param props Properties container
    -- @param key Key of property to retrieve
    -- @return Value for the given key or an empty string
    --
    function Get_Property(props: in Properties;
                            key: in Unbounded_String;
                        default: in Unbounded_String := To_Unbounded_String("")) return String is
    begin
        if props.Find(key) = Hash_Table.No_Element then
            if props.defaults.Find(key) /= Hash_Table.No_Element then
                return To_String(props.defaults.Element(key));
            else
                return To_String(default);
            end if;
        else
            return To_String(props.Element(key));
        end if;
    end Get_Property;

    --
    -- Get the named property from the properties container. If the requested
    -- key does not exist in the container then an empty string will be returned.
    -- @param props Properties container
    -- @param key Key of property to retrieve
    -- @return Value for the given key or an empty string
    --
    function Get_Property(props: in Properties; key: in String) return String is
    begin
        return props.Get_Property(To_Unbounded_String(key));
    end Get_Property;

    --
    -- Get the named property from the properties container. If the requested
    -- key does not exist in the container then the default value will be returned.
    -- @param props Properties container
    -- @param key Key of property to retrieve
    -- @return Value for the given key or the default value.
    --
    function Get_Property(props: in Properties; key: in String; default: in String) return String is
    begin
        return props.Get_Property(To_Unbounded_String(key), To_Unbounded_String(default));
    end Get_Property;

    --
    -- Set the given property to the specified value.
    -- @param props Properties container
    -- @param key Key of the desired property
    -- @param value Value for the desired property
    --
    procedure Set_Property(props: in out Properties; key: in Unbounded_String; value: in Unbounded_String) is
    begin
        if props.Find(Key) /= Hash_Table.No_Element then
            props.Replace(Key => key, New_Item => value);
        else
            props.Insert(Key => key, New_Item => value);
        end if;
    end Set_Property;

    --
    -- Set the given property to the specified value.
    -- @param props Properties container
    -- @param key Key of the desired property
    -- @param value Value for the desired property
    --
    procedure Set_Property(props: in out Properties; key: in String; value: in String) is
    begin
        props.Set_Property(To_Unbounded_String(key), To_Unbounded_String(value));
    end Set_Property;

    --
    -- Output all of the properties in standard key=value format to the specified
    -- output file.
    -- @param props Properties object
    -- @param output Output file
    --
    procedure List(props: in Properties; output: File_Type) is

        curs:   Hash_Table.Cursor;
        key:    Unbounded_String;
        value:  Unbounded_String;
    begin
        curs := props.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            value := props.Element(key);

            Put_Line(output, To_String(key) & "=" & To_String(value));

            curs := Hash_Table.Next(curs);
        end loop;
    end List;

    --
    -- Return a collection of all of the property names (Keys) in the
    -- Properties object.
    -- @param props Properties object
    -- @return Collection of property names (Keys)
    --
    function Property_Names(props: in Properties) return Key_Vector.Vector is
        keys: Key_Vector.Vector;
        curs: Hash_Table.Cursor;
        key:  Unbounded_String;
    begin
        curs := props.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            keys.Append(key);
            curs := Hash_Table.Next(curs);
        end loop;

        return keys;
    end Property_Names;

    --
    -- Return a collection of all of the property names (Keys) including all
    -- non-matching keys from the default Properties object.
    -- @param props Properties object
    -- @return Collection of property names (Keys)
    --
    function String_Property_Names(props: in Properties) return Key_Vector.Vector is
        keys: Key_Vector.Vector;
        curs: Hash_Table.Cursor;
        key:  Unbounded_String;
    begin
        curs := props.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            keys.Append(key);
            curs := Hash_Table.Next(curs);
        end loop;

        curs := props.defaults.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            if props.Find(key) = Hash_Table.No_Element then
                keys.Append(key);
            end if;
            curs := Hash_Table.Next(curs);
        end loop;

        return keys;
    end String_Property_Names;

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load() procedure.
    -- @param props Properties object
    -- @param output Output file handle
    -- @param comments Comments (if any)
    --
    procedure Store(props: in Properties; output: File_Type; comments: in String := "") is
        now:  Time := Clock;
        curs: Hash_Table.Cursor;
        key:  Unbounded_String;
    begin
        if comments'Length > 0 then
            Put_Line(output, "#" & comments);
        end if;

        Put_Line(output, "#" & Image(now, "%c"));

        curs := props.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            Put_Line(output, Escape(key) & "=" & Escape(props.Element(key), false));
            curs := Hash_Table.Next(curs);
        end loop;
    end Store;

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load() procedure.
    -- @param props Properties object
    -- @param filename Name of file to create
    -- @param comments Comments (if any)
    --
    procedure Store(props: in Properties; filename: in String; comments: in String := "") is
        output: File_Type;
    begin
        Create(output, Out_File, filename);
        props.Store(output, comments);
        Close(output);
    end Store;

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load_from_XML() procedure.
    -- @param props Properties object
    -- @param output Output file handle
    -- @param comments Comments (if any)
    --
    procedure Store_To_XML(props: Properties; output: File_Type; comments: in String := "") is
        now:  Time := Clock;
        curs: Hash_Table.Cursor;
        key:  Unbounded_String;
    begin
        Put_Line(output, "<?xml version=""1.0"" encoding=""UTF-8""?>");
        Put_Line(output, "<!DOCTYPE properties SYSTEM ""http://java.sun.com/dtd/properties.dtd"">");
        Put_Line(output, "<properties>");

        if comments'Length /= 0 then
            Put_Line(output, "<comment>" & comments & "</comment>");
        end if;

        curs := props.First;
        while curs /= Hash_Table.No_Element loop
            key := Hash_Table.Key(curs);
            Put_Line(output, "<entry key=""" & 
                Escape(key) & 
                """>" & 
                Escape(props.Element(key), false) & 
                "</entry>");
            curs := Hash_Table.Next(curs);
        end loop;

        Put_Line(output, "</properties>");
    end Store_To_XML;

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load_from_XML() procedure.
    -- @param props Properties object
    -- @param filename Name of file to create
    -- @param comments Comments (if any)
    --
    procedure Store_To_XML(props: Properties; filename: in String; comments: in String := "") is
        output: File_Type;
    begin
        Create(output, Out_File, filename);
        props.Store_To_XML(output, comments);
        Close(output);
    end Store_To_XML;

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param input Input file object
    --
    procedure Load(props: in out Properties; input: File_Type) is
        line: Unbounded_String;
        cpos: Natural;
    begin
        props.Clear;
        while not End_Of_File(input) loop
            Get_Line(input, line);
            Trim(line, Ada.Strings.Both);
            if Length(line) > 0 then
                if Element(line, 1) /= '#' and Element(line, 1) /= '!' then
                    declare
                        c: Character;
                        is_key: Natural := 0;
                        key, value: Unbounded_String;
                    begin
                        cpos := 1;
                        while cpos <= Length(line) loop
                            c := Element(line, cpos);
                            if c = '\' then
                                if cpos = Length(line) then
                                    Get_Line(input, line);
                                    Trim(line, Ada.Strings.Both);
                                    cpos := 0;
                                else
                                    cpos := cpos + 1;
                                    c := Element(line, cpos);
                                    if is_key = 0 then
                                        Append(key, c);
                                    else
                                        is_key := 2;
                                        Append(value, c);
                                    end if;
                                end if;
                            elsif is_key < 2 and (c = ':' or c = '=' or c = ' ') then
                                is_key := 1;
                            else
                                if is_key = 0 then
                                    Append(key, c);
                                else
                                    is_key := 2;
                                    Append(value, c);
                                end if;
                            end if;

                            cpos := cpos + 1;
                        end loop;

                        props.Set_Property(key, value);
                    end;
                end if;
            end if;
        end loop;
    end Load;

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param input Input file object
    --
    procedure Load(props: in out Properties; filename: in String) is
        input: File_Type;
    begin
        Open(input, In_File, filename);
        props.Load(input);
        Close(input);
    end Load;

    procedure Load_From_XML(props: in out Properties; input: File_Type) is
    begin
        props.Clear;
    end Load_From_XML;

    procedure Load_From_XML(props: in out Properties; filename: in String) is
        input: File_Type;
    begin
        Open(input, In_File, filename);
        props.Load_From_XML(input);
        Close(input);
    end Load_From_XML;

end Properties;
