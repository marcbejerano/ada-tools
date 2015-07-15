-- @(#)File:            logging-level.adb
-- @(#)Last changed:    Jun 2 2015 09:50:00
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

package body Logging.Level is

    LEVELS : constant array (1..8) of Level := (
        OFF, FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL_LEVELS);

    function "=" (aLevel: in Level; bLevel: in Level) return Boolean is
    begin
        return aLevel.intLevel = bLevel.intLevel;
    end "=";

    function ">" (aLevel: in Level; bLevel: in Level) return Boolean is
    begin
        return aLevel.intLevel > bLevel.intLevel;
    end ">";

    function "<" (aLevel: in Level; bLevel: in Level) return Boolean is
    begin
        return aLevel.intLevel < bLevel.intLevel;
    end "<";

    function ">=" (aLevel: in Level; bLevel: in Level) return Boolean is
    begin
        return aLevel > bLevel or aLevel = bLevel;
    end ">=";

    function "<=" (aLevel: in Level; bLevel: in Level) return Boolean is
    begin
        return aLevel < bLevel or aLevel = bLevel;
    end "<=";

    function Is_Less_Specific_Than(aLevel: in Level; aCompareLevel: in Level) return Boolean is
    begin
        return aLevel > aCompareLevel;
    end Is_Less_Specific_Than;

    function Is_More_Specific_Than(aLevel: in Level; aCompareLevel: in Level) return Boolean is
    begin
        return aLevel < aCompareLevel;
    end Is_More_Specific_Than;

    function To_Level(aName: in String) return Level is
    begin
        return To_Level(aName, DEBUG);
    end To_Level;

    function To_Level(aName: in String; aDefault : Level) return Level is
        aLevel : Level := aDefault;
    begin
        for idx in LEVELS'Range loop
            if (LEVELS(idx).name = aName) then
                aLevel := LEVELS(idx);
                exit;
            end if;
        end loop;
        return aLevel;
    end To_Level;

    function To_String(aLevel: in Level) return String is
    begin
        return To_String(aLevel.name);
    end To_String;

end Logging.Level;

