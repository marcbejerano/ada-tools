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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging.Appender;      use Logging.Appender;
with Logging.Level;         use Logging.Level;
with Logging.Event;         use Logging.Event;

package Logging.Logger is

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
        -- Add an output appender to this logger
        -- @param aAppender An appender
        --
        procedure Add_Appender(aAppender: in Appender.Appender_Class_Ptr);

        --
        -- Send the given log message with the specified priority to the logger's
        -- output handlers. If the specified priority is below the current minimum
        -- threshold then the message will be ignored.
        -- @param aLevel Priority level of the given message
        -- @param aMessage Message to send to logger
        --
        procedure Log(aLevel: in Level.Level; aMessage: in String);

        --
        -- Send the given Log_Event with the specified priority to the logger's
        -- output handlers. If the specified priority is below the current minimum
        -- threshold then the Log_Event will be ignored.
        -- @param aLevel Priority level of the given message
        -- @param aEvent Log_Event to send to the logger
        --
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
        Appenders: Appender_Vectors.Vector;

    end Logger;

    type Logger_Ptr is access all Logger;

    --
    -- Get the named logger from the logging pool
    -- @param aLoggerName Name of the logger to use
    -- @return Logger object
    --
    function Get_Logger(aLoggerName: in String) return Logger_Ptr;

    --
    -- Initialize the logging system with settings from the given 
    -- properties file.
    -- @param aFile_Name Name of the properties file
    --
    procedure Init_Logging(aFile_Name: in String);

end Logging.Logger;

