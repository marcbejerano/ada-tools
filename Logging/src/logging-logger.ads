-- @(#)File:            logging-logger.ads
-- @(#)Last changed:    June 12 2015 13:50:00
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

with Logging.Level;         use Logging.Level;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;

package Logging.Logger is

    --
    -- Class that describes a logging event
    --
    type Log_Event is tagged record
        Message     : Unbounded_String;
        File_Name   : Unbounded_String;
        Line_Number : Integer;
        Method_Name : Unbounded_String;
        Timestamp   : Time;
        Priority    : Level.Level;
    end record;

    --
    --
    --
    function New_Log_Event(Message     : in String;
                           File_Name   : in String := "";
                           Line_Number : in Natural := 0) return Log_Event;

    --
    --
    --
    function Format(Pattern: in String; Event: in Log_Event) return String;

    --
    -- Class the describes a protected logger instance.
    --
    protected type Logger is

        procedure Set_Level(L: in Level.Level);
        procedure Set_Pattern(S: in String);
        function Get_Pattern return String;

        procedure Log(L: in Level.Level; S: in String);
        procedure Log(L: in Level.Level; Event: in out Log_Event);

        procedure Fatal(S: in String);
        procedure Error(S: in String);
        procedure Warn (S: in String);
        procedure Info (S: in String);
        procedure Debug(S: in String);
        procedure Trace(S: in String);

        procedure Fatal(E: in out Log_Event);
        procedure Error(E: in out Log_Event);
        procedure Warn (E: in out Log_Event);
        procedure Info (E: in out Log_Event);
        procedure Debug(E: in out Log_Event);
        procedure Trace(E: in out Log_Event);

    private
        Min_Level: Level.Level := Level.WARN;
        Pattern: Unbounded_String := To_Unbounded_String("%-5p [%t]: %m%n");

    end Logger;

    type Logger_Ptr is access all Logger;

    --
    --
    --
    function Get_Logger(S: in String) return Logger_Ptr;

end Logging.Logger;

