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
        Entity      : Unbounded_String;
        Timestamp   : Time;
        Priority    : Level.Level;
    end record;

    --
    -- Create a new logging event object given all of the required parameters. The 
    -- Timestamp will be automatically set upon calling this function.
    -- @param Message Logging event message
    -- @param File_Name Filename where the logging event occurred
    -- @param Line_Number Line number where the logging event occurred
    -- @param Entity Enclosing entity where the logging event occurred
    -- @return A new Log_Event object
    --
    function New_Log_Event(Message     : in String;
                           File_Name   : in String := "";
                           Line_Number : in Natural := 0;
                           Entity      : in String := "") return Log_Event;

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
    -- |            | See the Ada package GNAT.CAlendar.Time_IO               |
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
    -- |     r      | Used to output the number of milliseconds elapsed from  |
    -- |            | the construction of the layout until the creation of    |
    -- |            | the logging event.                                      |
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
    function Format(aPattern: in String; aEvent: in Log_Event) return String;

    --
    -- Class that describes a protected logger instance.
    --
    protected type Logger is

        --
        -- Set the minimum priority level that will be displayed when
        -- log messages are sent to this logger.
        -- @param aLevel Priorty level
        --
        procedure Set_Level(aLevel: in Level.Level);

        --
        -- Set the logger output format pattern for all log messages. See Format()
        -- for formatting conversion codes.
        -- @param aPattern Message output pattern
        --
        procedure Set_Pattern(aPattern: in String);
        
        --
        -- Return the current message formatting pattern.
        -- @return Formatting pattern
        --
        function Get_Pattern return String;

        --
        -- Send the given log message with the specified priority to the logger's
        -- output handlers. If the specified priority is below the current minimum
        -- threshold then the message will be ignored.
        -- @param aLevel Priority level of the given message
        -- @param aMessage Message to send to logger
        --
        procedure Log(aLevel: in Level.Level; aMessage: in String);

        procedure Log(aLevel: in Level.Level; aEvent: in Log_Event);

        --
        -- Shortcut to the Log() function that will log an arbitrary message using
        -- the requested priority level.
        -- @param aMessage Message to log
        --
        procedure Fatal(aMessage: in String);
        procedure Error(aMessage: in String);
        procedure Warn (aMessage: in String);
        procedure Info (aMessage: in String);
        procedure Debug(aMessage: in String);
        procedure Trace(aMessage: in String);

        --
        -- Shortcut to the Log() function that will log a Log_Event using the
        -- requested priority level.
        -- @param aEvent Log event to send to the logger
        --
        procedure Fatal(aEvent: in Log_Event);
        procedure Error(aEvent: in Log_Event);
        procedure Warn (aEvent: in Log_Event);
        procedure Info (aEvent: in Log_Event);
        procedure Debug(aEvent: in Log_Event);
        procedure Trace(aEvent: in Log_Event);

    private
        Min_Priority_Level: Level.Level := Level.WARN;
        Pattern: Unbounded_String := To_Unbounded_String("%d{ISO8601} [%-5p] %m%n");

    end Logger;

    type Logger_Ptr is access all Logger;

    --
    -- Get the named logger from the logging pool
    -- @param aLoggerName Name of the logger to use
    -- @return Logger object
    --
    function Get_Logger(aLoggerName: in String) return Logger_Ptr;

end Logging.Logger;

