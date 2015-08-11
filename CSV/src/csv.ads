-- @(#)File:            csv.ads
-- @(#)Last changed:    Aug 6 2015 10:07:13
-- @(#)Purpose:         Character Separated Value (CSV) file support
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

-- RFC4180 https://tools.ietf.org/html/rfc4180

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package CSV is

    package ASU renames Ada.Strings.Unbounded;
    use ASU;
    package String_Vectors is new Ada.Containers.Vectors(Natural, Unbounded_String);
    use String_Vectors;

    type CSV_Record is new String_Vectors.Vector with null record;

    --
    -- Return the size of the CSV record. This function just returns the length
    -- of the container (vector).
    -- @param rec CSV record
    -- @return Size of the record, in columns
    --
    function Size(rec: in CSV_Record) return Natural;

    --
    -- Retrieve the CSV record value at the given index. If the given index falls
    -- outside of the range of available columns then a Constraint_Error will be
    -- raised.
    -- @param rec CSV record
    -- @param index Column index
    -- @return CSV column value
    --
    function Get(rec: in CSV_Record;
               index: in Natural) return String;

    --
    -- Update the CSV element at the given column index with the supplied value.
    -- If the given index falls outside of the range of available columns then
    -- a Constraint_Error will be raised.
    -- @param rec CSV record
    -- @param index Column index
    -- @param value New column value
    --
    procedure Set(rec: in out CSV_Record;
               index: in Natural;
               value: in String);

    type CSV_File is tagged limited private;

    --
    -- Open the CSV file for reading and/or writing. 
    -- @param csv CSV object
    -- @param file_name Name of the CSV file
    -- @param mode File mode (In_File, Out_File, or In_Out_File)
    -- @param has_header True if this CSV file has a header line
    --
    procedure Open(csv: in out CSV_File;
             file_name: in String;
                  mode: Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
            has_header: in Boolean := false);

    --
    -- Close the CSV file.
    -- @param csv CSV file
    --
    procedure Close(csv: in out CSV_File);

private

    type CSV_File is tagged limited record
        headers: CSV_Record;
        file:    Ada.text_IO.File_Type;
        delim:   String(1 .. 1) := ",";
    end record;

end CSV;

