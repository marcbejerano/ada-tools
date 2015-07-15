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

package body Logging.Logger is

    protected body Logger is

        procedure Set_Level(L: in Level.Level) is
        begin
            Min_Level := L;
        end Set_Level;

        procedure Set_Pattern(S: in String) is
        begin
            Pattern := To_Unbounded_String(S);
        end Set_Pattern;

        procedure Log(L: in Level.Level; S: in String) is
        begin
            if L <= Min_Level then
        		Put_Line(S);
            end if;
        end log;

    	procedure Fatal(S: in String) is
    	begin
            Log(Level.FATAL, S);
    	end Fatal;

    	procedure Error(S: in String) is
    	begin
            Log(Level.ERROR, S);
    	end Error;

    	procedure Warn(S: in String) is
    	begin
            Log(Level.WARN, S);
    	end Warn;

    	procedure Info(S: in String) is
    	begin
            Log(Level.INFO, S);
    	end Info;

    	procedure Debug(S: in String) is
    	begin
            Log(Level.DEBUG, S);
    	end Debug;

    	procedure Trace(S: in String) is
    	begin
            Log(Level.TRACE, S);
    	end Trace;
    end Logger;

    function Get_Logger(S: in String) return Logger_Ptr is
        aLogger: Logger_Ptr := null;
    begin
        aLogger := new Logger;
        return aLogger;
    end Get_Logger;

end Logging.Logger;
