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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Text_IO;           use Ada.Text_IO;
with Logging.Level;         use Logging.Level;

package body Logging.Appender is

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in Appender; aEvent: in Log_Event; aString: in String) is
    begin
        null;
    end Put;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in Console_Appender; aEvent: in Log_Event; aString: in String) is
    begin
        if aEvent.Priority = INFO then
            Put(Standard_Output, aString);
        else
            Put(Standard_Error, aString);
        end if;
    end Put;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in File_Appender; aEvent: in Log_Event; aString: in String) is
        aFile: File_Type;
    begin
        Open(aFile, Append_File, To_String(aAppender.File_Name));
        Put(aFile, Trim(aString, Ada.Strings.Right));
        Close(aFile);
    exception
        when Name_Error =>
            Create(aFile, Out_File, To_String(aAppender.File_Name));
            Put(aFile, Trim(aString, Ada.Strings.Right));
            Close(aFile);
    end Put;

end Logging.Appender;


