-- @(#)File:            properties.ads
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

with Ada.Containers.Hashed_Maps;    use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;

package Properties is

    type Properties is tagged private;

    package Key_Vector is new Ada.Containers.Vectors(Natural, Unbounded_String);
    use Key_Vector;

    --
    -- Properties tagged record Copy function. This function will take into
    -- account the added data member containing the default values.
    -- @param Source Properties object
    -- @param Capacity
    function Copy(Source: Properties; Capacity: Count_Type := 0) return Properties;

    --
    -- Create an empty Properties object.
    -- @return Properties object
    --
    function Create return Properties;

    --
    -- Create a Properties object with initial data provided by the
    -- given default container.
    -- @param defaults Default container of property data
    -- @return Properties object
    --
    function Create(defaults: in Properties) return Properties;

    --
    -- Get the named property from the properties container. If the requested
    -- key does not exist in the container then an empty string will be returned.
    -- @param props Properties container
    -- @param key Key of property to retrieve
    -- @return Value for the given key or an empty string
    --
    function Get_Property(props: in Properties; key: in String) return String;

    --
    -- Get the named property from the properties container. If the requested
    -- key does not exist in the container then the default value will be returned.
    -- @param props Properties container
    -- @param key Key of property to retrieve
    -- @return Value for the given key or the default value.
    --
    function Get_Property(props: in Properties;
                            key: in String;
                        default: in String) return String;

    function Get_Property(props: in Properties;
                            key: in Unbounded_String;
                        default: in Unbounded_String := To_Unbounded_String("")) return String;

    --
    -- Output all of the properties in standard key=value format to the specified
    -- output file.
    -- @param props Properties object
    -- @param output Output file
    --
    procedure List(props: in Properties; output: File_Type);

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param input Input file object
    --
    procedure Load(props: in out Properties; input: File_Type);

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param filename Name of file to load
    --
    procedure Load(props: in out Properties; filename: in String);

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param input Input file object
    --
    procedure Load_From_XML(props: in out Properties; input: File_Type);

    --
    -- Load the properties from the given input file object into
    -- the Properties object. All of the rules specified by the 
    -- Java SE 7 Properties class are enforced.<br>
    -- http://docs.oracle.com/javase/6/docs/api/java/util/Properties.html
    -- @param props Properties object
    -- @param filename Name of file to load
    --
    procedure Load_From_XML(props: in out Properties; filename: in String);

    --
    -- Return a collection of all of the property names (Keys) in the
    -- Properties object.
    -- @param props Properties object
    -- @return Collection of property names (Keys)
    --
    function Property_Names(props: in Properties) return Key_Vector.Vector;

    --
    -- Set the given property to the specified value.
    -- @param props Properties container
    -- @param key Key of the desired property
    -- @param value Value for the desired property
    --
    procedure Set_Property(props: in out Properties;
                             key: in String;
                           value: in String);

    procedure Set_Property(props: in out Properties;
                             key: in Unbounded_String;
                           value: in Unbounded_String);

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load() procedure.
    -- @param props Properties object
    -- @param output Output file handle
    -- @param comments Comments (if any)
    --
    procedure Store(props: in Properties; output: File_Type; comments: in String := "");

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load() procedure.
    -- @param props Properties object
    -- @param filename Name of file to create
    -- @param comments Comments (if any)
    --
    procedure Store(props: in Properties; filename: in String; comments: in String := "");

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load_from_XML() procedure.
    -- @param props Properties object
    -- @param output Output file handle
    -- @param comments Comments (if any)
    --
    procedure Store_To_XML(props: Properties; output: File_Type; comments: in String := "");

    --
    -- Store the contents of the Properties object to the output
    -- file in a format suitable for using the load_from_XML() procedure.
    -- @param props Properties object
    -- @param filename Name of file to create
    -- @param comments Comments (if any)
    --
    procedure Store_To_XML(props: Properties; filename: in String; comments: in String := "");

    --
    -- Return a collection of all of the property names (Keys) including all
    -- non-matching keys from the default Properties object.
    -- @param props Properties object
    -- @return Collection of property names (Keys)
    --
    function String_Property_Names(props: in Properties) return Key_Vector.Vector;

private

    --
    -- Hash function for the key
    --
    function Key_Hashed(key: Unbounded_String) return Hash_Type;
   
    --
    -- Hash Table package using Unbounded_String for both the key
    -- and the element
    --
    package Hash_Table is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Unbounded_String,
         Hash            => Key_Hashed,
         Equivalent_Keys => "=");

    --
    -- Properties container. Store the defaults container inside
    -- this object for resolving default values not found in
    -- a get_property() call (sans default value).
    --
    type Properties is new Hash_Table.Map with record
        defaults: Hash_Table.Map;
    end record;

end Properties;

