-- @(#)File:            properties_test.adb
-- @(#)Last changed:    Mar 18 2015 10:30:00
-- @(#)Purpose:         Java properties file support
-- @(#)Author:          Marc Bejerano <marcbejerano@gmail.com>
-- @(#)Copyright:       Copyright (C) 2015, Marc Bejerano, All Rights Reserved
-- @(#)Product:         None
-- @(#)License:         BSD3
--
-- Copyright (c) 2015, Marc Bejerano
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- * Neither the name of ada-tools nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
