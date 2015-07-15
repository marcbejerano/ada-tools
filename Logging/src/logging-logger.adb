-- @(#)File:            logging-logger.adb
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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.CAlendar;          use Ada.Calendar;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;

package body Logging.Logger is

    ISO8601_FORMAT  : constant Picture_String := "%Y-%m-%d %H:%M:%S,%i";
    ABSOLUTE_FORMAT : constant Picture_String := "%H:%M:%S,%i";
    DATE_FORMAT     : constant Picture_String := "%d %b %Y %H:%M:%S,%i";

    --
    --
    --
    function New_Log_Event(Message     : in String;
                           File_Name   : in String := "";
                           Line_Number : in Natural := 0) return Log_Event is
        Event : Log_Event;
    begin
        Event.Message := To_Unbounded_String(Message);
        Event.File_Name := Null_Unbounded_String;
        Event.Line_Number := Line_Number;
        if (File_Name'Length > 0) then
            Event.File_Name := To_Unbounded_String(File_Name);
        end if;

        Event.Method_Name := Null_Unbounded_String;
        Event.Timestamp := Clock;
        return Event;
    end New_Log_Event;

    --
    --
    --
    function Trim_and_Pad(Width, Max_Width: in Integer; Left_Justify: in Boolean; Text : in String) return String is
        t : Unbounded_String;
        s : Unbounded_String;
    begin
        if (Length(t) > Max_Width) then
            t := Unbounded_Slice(To_Unbounded_String(Text), 1, Max_Width);
        else
            t := To_Unbounded_String(Text);
        end if;

        if (Length(t) < Width) then
            if (Left_Justify) then
                s := Head(t, Width);
            else
                s := Tail(t, Width);
            end if;
        else
            s := t;
        end if;

        return To_String(s);
    end Trim_and_Pad;

    --
    --
    --
    -- %d{ISO8601} %-5p [%t]: %m%n
    --
    function Format(Pattern: in String; Event: in Log_Event) return String is
        ch  : Character;
        s   : Unbounded_String;
        idx : Natural;
    begin
        idx := Pattern'First;
        while idx <= Pattern'Last loop
            ch := Pattern(idx);
            if (ch = '%' and idx < Pattern'Last) then
                idx := idx + 1;

                declare
                    width     : Integer := 0;
                    max_width : Integer := 0;
                    left_just : Boolean := False;
                    param     : Unbounded_String := Null_Unbounded_String;
                begin
                    ch := Pattern(idx);

                    -- check for a Last modifier
                    if (ch = '-' or ch = '.' or ch in '0'..'9') then
                        if (ch = '-') then
                            left_just := True;
                            idx := idx + 1;
                        end if;

                        -- determine width
                        while (idx <= Pattern'Last and Pattern(idx) in '0'..'9') loop
                            ch := Pattern(idx);
                            width := (width * 10) + (Character'Pos(ch) - Character'Pos('0'));
                            idx := idx + 1;
                        end loop;

                        -- max_width specified
                        if (Pattern(idx) = '.') then
                            idx := idx + 1;

                            while (idx <= Pattern'Last and Pattern(idx) in '0'..'9') loop
                                ch := Pattern(idx);
                                max_width := (max_width * 10) + (Character'Pos(ch) - Character'Pos('0'));
                                idx := idx + 1;
                            end loop;
                        end if;

                        ch := Pattern(idx);
                    end if;

                    -- determine the pattern parameter
                    if (idx + 1 < Pattern'Last) then
                        if (Pattern(idx + 1) = '{') then
                            idx := idx + 1;

                            if (idx <= Pattern'Last) then
                                idx := idx + 1;
                                while (idx < Pattern'Last and Pattern(idx) /= '}') loop
                                    Append(param, Pattern(idx));
                                    idx := idx + 1;
                                end loop;
                            end if;
                        end if;
                    end if;

                    -- process the conversion character
                    case ch is
                        when 'c' => null;
                        when 'C' => null;
                        when 'd' =>
                            if (param = "ISO8601" or param = Null_Unbounded_String) then
                                Append(s, Trim_and_Pad(width, max_width, left_just, Image(Event.Timestamp, ISO8601_FORMAT)));
                            elsif (param = "DATE") then
                                Append(s, Trim_and_Pad(width, max_width, left_just, Image(Event.Timestamp, DATE_FORMAT)));
                            elsif (param = "ABSOLUTE") then
                                Append(s, Trim_and_Pad(width, max_width, left_just, Image(Event.Timestamp, ABSOLUTE_FORMAT)));
                            else
                                Append(s, Trim_and_Pad(width, max_width, left_just, Image(Event.Timestamp, ISO_DATE)));
                            end if;
                        when 'F' => null;
                            if (Event.File_Name /= Null_Unbounded_String) then
                                Append(s, Trim_and_Pad(width, max_width, left_just, To_String(Event.File_Name)));
                            end if;
                        when 'l' => null;
                        when 'L' => null;
                            if (Event.Line_Number >= 0) then
                                Append(s, Trim_and_Pad(width, max_width, left_just, Natural'Image(Event.Line_Number)));
                            end if;
                        when 'm' => null;
                            if (Event.Message /= Null_Unbounded_String) then
                                Append(s, Trim_and_Pad(width, max_width, left_just, To_String(Event.Message)));
                            end if;
                        when 'M' => null;
                            if (Event.Method_Name /= Null_Unbounded_String) then
                                Append(s, Trim_and_Pad(width, max_width, left_just, To_String(Event.Method_Name)));
                            end if;
                        when 'n' =>
                            Append(s, ASCII.CR & ASCII.LF);
                        when 'p' => null;
                            Append(s, Trim_and_Pad(width, max_width, left_just, To_String(Event.Priority)));
                        when 'r' => null;
                        when 't' => null;
                        when 'x' => null;
                        when 'X' => null;
                        when '%' => null;
                        when others =>
                            Append(s, ch);
                    end case;
                end;
            else
                Append(s, Pattern(idx));
            end if;

            idx := idx + 1;
        end loop;
        return To_String(s);
    end Format;

    --
    --
    --
    protected body Logger is

        --
        --
        --
        procedure Set_Level(L: in Level.Level) is
        begin
            Min_Level := L;
        end Set_Level;

        --
        --
        --
        procedure Set_Pattern(S: in String) is
        begin
            Pattern := To_Unbounded_String(S);
        end Set_Pattern;

        --
        --
        --
        function Get_Pattern return String is
        begin
            return To_String(Pattern);
        end Get_Pattern;

        --
        --
        --
        procedure Log(L: in Level.Level; S: in String) is
            event : Log_Event;
        begin
            if L <= Min_Level then
                event.File_Name := Null_Unbounded_String;
                event.Method_Name := Null_Unbounded_String;
                event.Line_Number := -1;
                event.Priority := L;
                event.Message := To_Unbounded_String(S);
        		Put(Format(To_String(Pattern), event));
            end if;
        end log;
        
        --
        --
        --
        procedure Log(L: in Level.Level; Event: in out Log_Event) is
        begin
            if L <= Min_Level then
                Event.Priority := L;
        		Put(Format(To_String(Pattern), event));
            end if;
        end log;

        --
        --
        --
    	procedure Fatal(S: in String) is
    	begin
            Log(Level.FATAL, S);
    	end Fatal;

        --
        --
        --
    	procedure Error(S: in String) is
    	begin
            Log(Level.ERROR, S);
    	end Error;

        --
        --
        --
    	procedure Warn(S: in String) is
    	begin
            Log(Level.WARN, S);
    	end Warn;

        --
        --
        --
    	procedure Info(S: in String) is
    	begin
            Log(Level.INFO, S);
    	end Info;

        --
        --
        --
    	procedure Debug(S: in String) is
    	begin
            Log(Level.DEBUG, S);
    	end Debug;

        --
        --
        --
    	procedure Trace(S: in String) is
    	begin
            Log(Level.TRACE, S);
    	end Trace;

        --
        --
        --
    	procedure Fatal(E: in out Log_Event) is
    	begin
            Log(Level.FATAL, E);
    	end Fatal;

        --
        --
        --
    	procedure Error(E: in out Log_Event) is
    	begin
            Log(Level.ERROR, E);
    	end Error;

        --
        --
        --
    	procedure Warn(E: in out Log_Event) is
    	begin
            Log(Level.WARN, E);
    	end Warn;

        --
        --
        --
    	procedure Info(E: in out Log_Event) is
    	begin
            Log(Level.INFO, E);
    	end Info;

        --
        --
        --
    	procedure Debug(E: in out Log_Event) is
    	begin
            Log(Level.DEBUG, E);
    	end Debug;

        --
        --
        --
    	procedure Trace(E: in out Log_Event) is
    	begin
            Log(Level.TRACE, E);
    	end Trace;
    end Logger;

    --
    --
    --
    function Get_Logger(S: in String) return Logger_Ptr is
        aLogger: Logger_Ptr := null;
    begin
        aLogger := new Logger;
        return aLogger;
    end Get_Logger;

end Logging.Logger;
