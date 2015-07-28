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

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Containers.Hashed_Maps;    use Ada.Containers;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with GNAT.String_Split;
with Properties;                    use Properties;

package body Logging.Logger is

    --
    -- Hash the given key into a Hash object.
    -- @param key Key to hash
    -- @return Hash object
    --
    function Key_Hashed(key: in Unbounded_String) return Hash_Type is
    begin
        return Hash(key);
    end Key_Hashed;
   
    package Logger_Table is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Logger_Ptr,
         Hash            => Key_Hashed,
         Equivalent_Keys => "=");

    use type Logger_Table.Cursor;

    package Appender_Table is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Appender.Appender_Class_Ptr,
         Hash            => Key_Hashed,
         Equivalent_Keys => "=");

    use type Appender_Table.Cursor;

    Console         : Appender_Class_Ptr;
    Console_Logger  : Logger_Ptr;
    Loggers         : Logger_Table.Map;
    Appenders       : Appender_Table.Map;

    --
    -- Class that describes a protected logger instance.
    --
    protected body Logger is

        --
        -- Set the minimum priority level that will be displayed when
        -- log messages are sent to this logger.
        -- @param aLevel Priorty level
        --
        procedure Set_Level(aLevel: in Level.Level) is
        begin
            Min_Priority_Level := aLevel;
        end Set_Level;

        --
        -- Add an output appender to this logger
        -- @param aAppender An appender
        --
        procedure Add_Appender(aAppender: in Appender.Appender_Class_Ptr) is
        begin
            Appenders.Append(aAppender);
        end Add_Appender;

        --
        -- Return the vector of Appenders contained in this logger.
        -- @return Vector of appenders
        --
        function Get_Appenders return Appender.Appender_Vectors.Vector is
        begin
            return Appenders;
        end Get_Appenders;

        --
        -- Send the given log message with the specified priority to the logger's
        -- output handlers. If the specified priority is below the current minimum
        -- threshold then the message will be ignored.
        -- @param aLevel Priority level of the given message
        -- @param aMessage Message to send to logger
        --
        procedure Log(aLevel: in Level.Level; aMessage: in String) is
            event : Log_Event;
        begin
            event.File_Name := Null_Unbounded_String;
            event.Entity := Null_Unbounded_String;
            event.Line_Number := -1;
            event.Priority := aLevel;
            event.Message := To_Unbounded_String(aMessage);
            event.Timestamp := Clock;

            Log(aLevel, event);
        end log;
        
        --
        -- Send the given Log_Event with the specified priority to the logger's
        -- output handlers. If the specified priority is below the current minimum
        -- threshold then the Log_Event will be ignored.
        -- @param aLevel Priority level of the given message
        -- @param aEvent Log_Event to send to the logger
        --
        procedure Log(aLevel: in Level.Level; aEvent: in Log_Event) is
            tmpEvent : Log_Event := aEvent;
        begin
            if aLevel <= Min_Priority_Level then
                tmpEvent.Priority := aLevel;

                if Appenders.Is_Empty then
                    Console.Put(tmpEvent);
                else
                    declare
                        aCursor: Appender_Vectors.Cursor := Appenders.First;
                        aAppender: Appender.Appender_Class_Ptr;
                    begin
                        while Appender_Vectors.Has_Element(aCursor) loop
                            aAppender := Appender_Vectors.Element(aCursor);
                            aAppender.Put(tmpEvent);
                            aCursor := Appender_Vectors.Next(aCursor);
                        end loop;
                    end;
                end if;
            end if;
        end log;

        --
        -- Shortcut to the Log() function that will log an arbitrary message using
        -- the requested priority level.
        -- @param aMessage Message to log
        --
        procedure Fatal(aMessage: in String) is begin Log(Level.FATAL, aMessage); end Fatal;
        procedure Error(aMessage: in String) is begin Log(Level.ERROR, aMessage); end Error;
        procedure Warn (aMessage: in String) is begin Log(Level.WARN,  aMessage); end Warn;
        procedure Info (aMessage: in String) is begin Log(Level.INFO,  aMessage); end Info;
        procedure Debug(aMessage: in String) is begin Log(Level.DEBUG, aMessage); end Debug;
        procedure Trace(aMessage: in String) is begin Log(Level.TRACE, aMessage); end Trace;

        --
        -- Shortcut to the Log() function that will log a Log_Event using the
        -- requested priority level.
        -- @param aEvent Log event to send to the logger
        --
        procedure Fatal(aEvent: in Log_Event) is begin Log(Level.FATAL, aEvent); end Fatal;
        procedure Error(aEvent: in Log_Event) is begin Log(Level.ERROR, aEvent); end Error;
        procedure Warn (aEvent: in Log_Event) is begin Log(Level.WARN,  aEvent); end Warn;
        procedure Info (aEvent: in Log_Event) is begin Log(Level.INFO,  aEvent); end Info;
        procedure Debug(aEvent: in Log_Event) is begin Log(Level.DEBUG, aEvent); end Debug;
        procedure Trace(aEvent: in Log_Event) is begin Log(Level.TRACE, aEvent); end Trace;
    end Logger;

    --
    -- Get the named logger from the logging pool
    -- @param aLoggerName Name of the logger to use
    -- @return Logger object
    --
    function Get_Logger(aLoggerName: in String) return Logger_Ptr is
        aLogger: Logger_Ptr := Console_Logger;
        aKey:    constant Unbounded_String := To_Unbounded_String(aLoggerName);
    begin
        if Loggers.Find(aKey) /= Logger_Table.No_Element then
            aLogger := Loggers.Element(aKey);
        end if;
        return aLogger;
    end Get_Logger;

    function Starts_With(aString: in String; aFindString: in String) return Boolean is
        Result : Boolean := False;
    begin
        if aString'Length >= aFindString'Length and then
            aString(aString'First .. aString'First + aFindString'Length - 1) = aFindString then
                Result := True;
        end if;
        return Result;
    end Starts_With;

    --
    -- Constants for the intialization function
    --
    APPENDER_PREFIX:  constant String := "appender.";

    type StrArray is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

    function String_Split(aString: in String; aSeparator: in String := ",") return StrArray is
        Tokens : GNAT.String_Split.Slice_Set;
    begin
        GNAT.String_Split.Create (S => Tokens,
                               From => aString,
                         Separators => aSeparator,
                               Mode => GNAT.String_Split.Single);

        declare
            Output : StrArray(1 .. Natural(GNAT.String_Split.Slice_Count(Tokens)));
        begin
            for I in Output'Range loop
                Output (I) :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                        (GNAT.String_Split.Slice
                            (Tokens, GNAT.String_Split.Slice_Number (I)));
            end loop;
            return Output;
        end;
    end String_Split;

    --
    -- Initialize the logging system with settings from the given 
    -- properties file.
    -- @param aFile_Name Name of the properties file
    --
    procedure Init_Logging(aFile_Name: in String) is
        aProps:     Properties.Properties;
        aKeys:      Properties.Key_Vector.Vector;
    begin
        Load(aProps, aFile_Name);
        aKeys := aProps.Property_Names;

        --
        -- THIS NEEDS TO BE OPTIMIZED!!
        --

        -- first pass: find all appenders and create them in an associative container
        for key_index in 0..Integer(aKeys.Length) - 1 loop
            declare
                aKeyString: constant String := To_String(aKeys(key_index));
            begin
                -- check for an appender line
                if Starts_With(aKeyString, APPENDER_PREFIX) and then Count(aKeyString, ".") = 1 then
                    declare
                        aName: constant Unbounded_String := To_Unbounded_String(aKeyString(Index(aKeyString, ".") + 1 .. aKeyString'Last));
                        aValue: constant Unbounded_String := To_Unbounded_String(aProps.Get_Property(aKeyString));
                        aAppender: Appender.Appender_Class_Ptr := null;
                    begin
                        if aValue = "ConsoleAppender" then
                            aAppender := new Console_Appender;
                        elsif aValue = "FileAppender" then
                            aAppender := new File_Appender;
                        end if;
                        if aAppender /= null then
                            if Appenders.Find(aName) /= Appender_Table.No_Element then
                                Appenders.Replace(Key => aName, New_Item => aAppender);
                            else
                                Appenders.Insert(Key => aName, New_Item => aAppender);
                            end if;
                        end if;
                    end;
                end if;
            end;
        end loop;

        -- second pass: update the appenders with all of their properties
        for key_index in 0..Integer(aKeys.Length) - 1 loop
            declare
                aKeyString: constant String := To_String(aKeys(key_index));
            begin
                if Starts_With(aKeyString, APPENDER_PREFIX) and then Count(aKeyString, ".") > 1 then
                    declare
                        aFirstDot:  constant Natural := Index(aKeyString, ".");
                        aSecondDot: constant Natural := Index(aKeyString, ".", aFirstDot + 1);
                        aName:      constant Unbounded_String := To_Unbounded_String(aKeyString(aFirstDot + 1 .. aSecondDot - 1));
                        aParam:     constant Unbounded_String := To_Unbounded_String(aKeyString(aSecondDot + 1 .. aKeyString'Length));
                        aValue:     constant Unbounded_String := To_Unbounded_String(aProps.Get_Property(aKeyString));
                        aAppender:  Appender.Appender_Class_Ptr := null;
                    begin
                        if Appenders.Find(aName) /= Appender_Table.No_Element then
                            aAppender := Appenders.Element(aName);
                            if aParam = "layout" then
                                aAppender.Set_Pattern(To_String(aValue));
                            elsif aParam = "filename" then
                                Appender.File_Appender_Ptr(aAppender).Set_File_Name(To_String(aValue));
                            end if;
                        end if;
                    end;
                end if;
            end;
        end loop;

        -- third pass: create the loggers
        for key_index in 0..Integer(aKeys.Length) - 1 loop
            declare
                aKeyString: constant String := To_String(aKeys(key_index));
                aValue:     constant Unbounded_String := To_Unbounded_String(aProps.Get_Property(aKeyString));
                aTokens:    constant StrArray := String_Split(To_String(aValue));
            begin
                if aKeyString'Length > 0 and not Starts_With(aKeyString, APPENDER_PREFIX) then
                    declare
                        aLogger: constant Logger_Ptr := new Logger;
                    begin
                        aLogger.Set_Level(Level.To_Level(To_String(aTokens(1))));
                        for adx in 2 .. aTokens'Length loop
                            if Appenders.Find(aTokens(adx)) /= Appender_Table.No_Element then
                                aLogger.Add_Appender(Appenders.Element(aTokens(adx)));
                            end if;
                        end loop;

                        if Loggers.Find(To_Unbounded_String(aKeyString)) /= Logger_Table.No_Element then
                            Loggers.Replace(Key => To_Unbounded_String(aKeyString), New_Item => aLogger);
                        else
                            Loggers.Insert(Key => To_Unbounded_String(aKeyString), New_Item => aLogger);
                        end if;
                    end;
                end if;
            end;
        end loop;
    end Init_Logging;

begin
    Console := new Console_Appender;
    Console.Set_Pattern("%d{ISO8601} %m%n");
    Console_Logger := new Logger;
    Console_Logger.Add_Appender(Console);
    Console_Logger.Set_Level(DEBUG);
end Logging.Logger;

