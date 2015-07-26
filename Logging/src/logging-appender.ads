-- @(#)File:            logging-appender.ads
-- @(#)Last changed:    Jul 21 2015 13:08:00
-- @(#)Purpose:         Application and system logging
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;    use Ada.Containers;
with Logging.Event;             use Logging.Event;

package Logging.Appender is

    --
    -- Base appender object
    --
    type Appender is tagged record
        Pattern : Unbounded_String := To_Unbounded_String("%d{ISO8601} [%-5p] %m%n");
    end record;

    --
    -- Set the logger output format pattern for all log messages. See Format()
    -- for formatting conversion codes.
    -- @param aPattern Message output pattern
    --
    procedure Set_Pattern(aAppender: in out Appender; aPattern: in String);
    
    --
    -- Return the current message formatting pattern.
    -- @return Formatting pattern
    --
    function Get_Pattern(aAppender: in Appender) return String;

    --
    -- Output the given string top the File_Appender. This being the
    -- base Appender Put() function ... it does nothing. Just a stub.
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString MEssage to log
    --
    procedure Put(aAppender: in Appender; aEvent: in Log_Event);

    --
    -- Console appender object. All output goes to the console
    --
    type Console_Appender is new Appender with null record;
    type Console_Appender_Ptr is access all Console_Appender;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString MEssage to log
    --
    procedure Put(aAppender: in Console_Appender; aEvent: in Log_Event);

    --
    -- File appender object. All output is appended to the named file.
    --
    type File_Appender is new Appender with record
        File_Name: Unbounded_String;
    end record;
    type File_Appender_Ptr is access all File_Appender;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString MEssage to log
    --
    procedure Put(aAppender: in File_Appender; aEvent: in Log_Event);

    --
    -- Set the file appender output filename
    -- @param aAppender Appender to update
    -- @param aFileName Name of file
    --
    procedure Set_File_Name(aAppender: in out File_Appender; aFileName: in String);

    --
    -- Pointer to an object of class Appender
    --
    type Appender_Class_Ptr is access all Appender'Class;

    --
    -- Aliased vector container of Appenders
    --
    package Appender_Vectors is new Ada.Containers.Vectors(
        Element_Type => Appender_Class_Ptr,
        Index_Type   => Positive);

end Logging.Appender;

