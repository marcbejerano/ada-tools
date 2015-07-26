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

with Ada.Characters.Latin_1;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;
with Logging.Level;         use Logging.Level;

package body Logging.Appender is

    ISO8601_FORMAT  : constant Picture_String := "%Y-%m-%d %H:%M:%S,%i";
    ABSOLUTE_FORMAT : constant Picture_String := "%H:%M:%S,%i";
    DATE_FORMAT     : constant Picture_String := "%d %b %Y %H:%M:%S,%i";

    --
    -- Apply the trimming and/or padding to the given string based on justification,
    -- width, and maximum width.
    -- @param Width Width of the target string
    -- @param Max_Width Maximum width of the allowed string (crop if Text is longer)
    -- @param Left_Justify Justify the string left or right within Width
    -- @param Text Text to trim/pad/justify
    -- @return Modified text
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
    -- Format the given Log_Event according to the pattern defined by the Pattern
    -- argument. The Pattern formatting follows the same rules as the Log4J 1.2 API
    -- formatting markers.
    --
    -- A flexible layout configurable with pattern string.  The goal of this function
    -- is to format a Log_Event and return the results as a String. The results depend
    -- on the conversion pattern.
    --
    -- The conversion pattern is closely related to the conversion pattern of the
    -- printf function in C. A conversion pattern is composed of literal text and
    -- format control expressions called conversion specifiers.
    --
    -- You are free to insert any literal text within the conversion pattern.
    --
    -- Each conversion specifier starts with a percent sign (%) and is followed by
    -- optional format modifiers and a conversion character. The conversion character
    -- specifies the type of data, e.g. category, priority, date, thread name. The
    -- format modifiers control such things as field width, padding, left and right
    -- justification. The following is a simple example.
    --
    -- Let the conversion pattern be "%-5p [%t]: %m%n" and assume that the logging
    -- environment was set to use a Pattern. Then the statements
    --
    --   Logger root = Get_Logger("foo");
    --   root.debug("Message 1");
    --   root.warn("Message 2");
    --
    -- would yield the output
    --
    --   DEBUG [main]: Message 1
    --   WARN  [main]: Message 2
    --                     
    -- Note that there is no explicit separator between text and conversion 
    -- specifiers. The pattern parser knows when it has reached the end of a
    -- conversion specifier when it reads a conversion character. In the
    -- example above the conversion specifier %-5p means the priority of the
    -- logging event should be left justified to a width of five characters.
    -- The recognized conversion characters are:
    --
    -- +------------+---------------------------------------------------------+
    -- | Conversion |                                                         |
    -- |  Character |                        Effect                           |
    -- +------------+---------------------------------------------------------+
    -- |     c      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     C      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     d      | Used to output the date of the logging event. The date  |
    -- |            | conversion specifier may be followed by a date format   |
    -- |            | specifier enclosed between braces. For example,         |
    -- |            | %d{HH:mm:ss,SSS} or %d{dd MMM yyyy HH:mm:ss,SSS}. If no |
    -- |            | date format specifier is given then ISO8601 format is   |
    -- |            | assumed.                                                |
    -- |            |                                                         |
    -- |            | See the Ada package GNAT.Calendar.Time_IO               |
    -- |            |                                                         |
    -- |            | For better results it is recommended to use one of the  |
    -- |            | strings "ABSOLUTE", "DATE" and "ISO8601".               |
    -- +------------+---------------------------------------------------------+
    -- |     F      | Used to output the file name where the logging request  |
    -- |            | was issued.                                             |
    -- +------------+---------------------------------------------------------+
    -- |     l      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     L      | Used to output the line number from where the logging   |
    -- |            | request was issued.                                     |
    -- +------------+---------------------------------------------------------+
    -- |     m      | Used to output the application supplied message         |
    -- |            | associated with the logging event.                      |
    -- +------------+---------------------------------------------------------+
    -- |     M      | Used to output the method (enclosing entity) name where |
    -- |            | the logging request was issued.                         |
    -- +------------+---------------------------------------------------------+
    -- |     n      | Outputs the line separator strings "\n"                 |
    -- +------------+---------------------------------------------------------+
    -- |     p      | Used to output the priority of the logging event.       |
    -- +------------+---------------------------------------------------------+
    -- |     r      | Used to output the current time in milliseconds         |
    -- +------------+---------------------------------------------------------+
    -- |     t      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     x      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     X      | Not used                                                |
    -- +------------+---------------------------------------------------------+
    -- |     %      | The sequence %% outputs a single percent sign           |
    -- +------------+---------------------------------------------------------+
    --
    -- @param Pattern Formatting pattern
    -- @param Event Logging event
    --
    function Format(aPattern: in String; aEvent: in Log_Event) return String is
        ch  : Character;
        s   : Unbounded_String;
        idx : Natural;
    begin
        idx := aPattern'First;
        while idx <= aPattern'Last loop
            ch := aPattern(idx);
            if (ch = '%' and idx < aPattern'Last) then
                idx := idx + 1;

                declare
                    width     : Integer := 0;
                    max_width : Integer := 0;
                    left_just : Boolean := False;
                    param     : Unbounded_String := Null_Unbounded_String;
                begin
                    ch := aPattern(idx);

                    -- check for a Last modifier
                    if (ch = '-' or ch = '.' or ch in '0'..'9') then
                        if (ch = '-') then
                            left_just := True;
                            idx := idx + 1;
                        end if;

                        -- determine width
                        while (idx <= aPattern'Last and aPattern(idx) in '0'..'9') loop
                            ch := aPattern(idx);
                            width := (width * 10) + (Character'Pos(ch) - Character'Pos('0'));
                            idx := idx + 1;
                        end loop;

                        -- max_width specified
                        if (aPattern(idx) = '.') then
                            idx := idx + 1;

                            while (idx <= aPattern'Last and aPattern(idx) in '0'..'9') loop
                                ch := aPattern(idx);
                                max_width := (max_width * 10) + (Character'Pos(ch) - Character'Pos('0'));
                                idx := idx + 1;
                            end loop;
                        end if;

                        ch := aPattern(idx);
                    end if;

                    -- determine the pattern parameter
                    if (idx + 1 < aPattern'Last) then
                        if (aPattern(idx + 1) = '{') then
                            idx := idx + 1;

                            if (idx <= aPattern'Last) then
                                idx := idx + 1;
                                while (idx < aPattern'Last and aPattern(idx) /= '}') loop
                                    Append(param, aPattern(idx));
                                    idx := idx + 1;
                                end loop;
                            end if;
                        end if;
                    end if;

                    -- process the conversion character
                    case ch is
                        when 'c' => null; -- not used
                        when 'C' => null; -- not used
                        when 'd' => if (param = "ISO8601" or param = Null_Unbounded_String) then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, Image(aEvent.Timestamp, ISO8601_FORMAT)));
                                    elsif (param = "DATE") then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, Image(aEvent.Timestamp, DATE_FORMAT)));
                                    elsif (param = "ABSOLUTE") then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, Image(aEvent.Timestamp, ABSOLUTE_FORMAT)));
                                    else
                                        Append(s, Trim_and_Pad(width, max_width, left_just, Image(aEvent.Timestamp, ISO_DATE)));
                                    end if;
                        when 'F' => if (aEvent.File_Name /= Null_Unbounded_String) then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, To_String(aEvent.File_Name)));
                                    end if;
                        when 'l' => null; -- not used
                        when 'L' => if (aEvent.Line_Number >= 0) then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, Natural'Image(aEvent.Line_Number)));
                                    end if;
                        when 'm' => if (aEvent.Message /= Null_Unbounded_String) then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, To_String(aEvent.Message)));
                                    end if;
                        when 'M' => if (aEvent.Entity /= Null_Unbounded_String) then
                                        Append(s, Trim_and_Pad(width, max_width, left_just, To_String(aEvent.Entity)));
                                    end if;
                        when 'n' => Append(s, Ada.Characters.Latin_1.LF);
                        when 'p' => Append(s, Trim_and_Pad(width, max_width, left_just, To_String(aEvent.Priority)));
                        when 'r' => null; -- not used
                        when 't' => null; -- not used
                        when 'x' => null; -- not used
                        when 'X' => null; -- not used
                        when '%' => Append(s, ch);
                        when others =>
                            Append(s, ch);
                    end case;
                end;
            else
                Append(s, aPattern(idx));
            end if;

            idx := idx + 1;
        end loop;
        return To_String(s);
    end Format;

    --
    -- Set the logger output format pattern for all log messages. See Format()
    -- for formatting conversion codes.
    -- @param aPattern Message output pattern
    --
    procedure Set_Pattern(aAppender: in out Appender; aPattern: in String) is
    begin
        aAppender.Pattern := To_Unbounded_String(aPattern);
    end Set_Pattern;
    
    --
    -- Return the current message formatting pattern.
    -- @return Formatting pattern
    --
    function Get_Pattern(aAppender: in Appender) return String is
    begin
        return To_String(aAppender.Pattern);
    end Get_Pattern;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in Appender; aEvent: in Log_Event) is
    begin
        null;
    end Put;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in Console_Appender; aEvent: in Log_Event) is
        aText: constant String := Format(aAppender.Get_Pattern, aEvent);
    begin
        if aEvent.Priority = INFO then
            Put(Standard_Output, aText);
        else
            Put(Standard_Error, aText);
        end if;
    end Put;

    --
    -- Output the given string top the File_Appender
    -- @param aAppender Appender to write to
    -- @param aEvent Event to log
    -- @param aString String to write
    --
    procedure Put(aAppender: in File_Appender; aEvent: in Log_Event) is
        aFile: File_Type;
        aText: constant String := Format(aAppender.Get_Pattern, aEvent);
    begin
        Open(aFile, Append_File, To_String(aAppender.File_Name));
        Put(aFile, aText);
        Close(aFile);
    exception
        when Name_Error =>
            Create(aFile, Out_File, To_String(aAppender.File_Name));
            Put(aFile, aText);
            Close(aFile);
    end Put;

    --
    -- Set the file appender output filename
    -- @param aAppender Appender to update
    -- @param aFileName Name of file
    --
    procedure Set_File_Name(aAppender: in out File_Appender; aFileName: in String) is
    begin
        aAppender.File_Name := To_Unbounded_String(aFileName);
    end Set_File_Name;

end Logging.Appender;


