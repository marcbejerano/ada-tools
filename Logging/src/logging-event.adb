-- @(#)File:            logging-logevent.ads
-- @(#)Last changed:    July 21 2015 14:51:00
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

package body Logging.Event is

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
                           Entity      : in String := "") return Log_Event is
        Event : Log_Event;
    begin
        Event.Message := To_Unbounded_String(Message);
        Event.File_Name := Null_Unbounded_String;
        Event.Line_Number := Line_Number;
        if (File_Name'Length > 0) then
            Event.File_Name := To_Unbounded_String(File_Name);
        end if;

        Event.Entity    := To_Unbounded_String(Entity);
        Event.Timestamp := Clock;
        return Event;
    end New_Log_Event;

end Logging.Event;

