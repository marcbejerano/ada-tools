-- @(#)File:            logging-level.ads
-- @(#)Last changed:    Jun 2 2015 09:37:00
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

package Logging.Level is

    INT_OFF   : constant Integer := 0;
    INT_FATAL : constant Integer := 100;
    INT_ERROR : constant Integer := 200;
    INT_WARN  : constant Integer := 300;
    INT_INFO  : constant Integer := 400;
    INT_DEBUG : constant Integer := 500;
    INT_TRACE : constant Integer := 600;
    INT_ALL   : constant Integer := Integer'Last;

    --
    -- Class that describes a unique logging level.
    --
    type Level is tagged record
        intLevel : Integer;
        name     : Unbounded_String;
    end record;

    function "=" (aLevel: in Level; bLevel: in Level) return Boolean;
    function ">" (aLevel: in Level; bLevel: in Level) return Boolean;
    function "<" (aLevel: in Level; bLevel: in Level) return Boolean;
    function ">=" (aLevel: in Level; bLevel: in Level) return Boolean;
    function "<=" (aLevel: in Level; bLevel: in Level) return Boolean;

    function Is_Less_Specific_Than(aLevel: in Level; aCompareLevel: in Level) return Boolean;
    function Is_More_Specific_Than(aLevel: in Level; aCompareLevel: in Level) return Boolean;
    function To_Level(aName: in String) return Level;
    function To_Level(aName: in String; aDefault : Level) return Level;
    function To_String(aLevel: in Level) return String;

    OFF        : constant Level := (intLevel => INT_OFF,   name => To_Unbounded_String("OFF"));
    FATAL      : constant Level := (intLevel => INT_FATAL, name => To_Unbounded_String("FATAL"));
    ERROR      : constant Level := (intLevel => INT_ERROR, name => To_Unbounded_String("ERROR"));
    WARN       : constant Level := (intLevel => INT_WARN,  name => To_Unbounded_String("WARN"));
    INFO       : constant Level := (intLevel => INT_INFO,  name => To_Unbounded_String("INFO"));
    DEBUG      : constant Level := (intLevel => INT_DEBUG, name => To_Unbounded_String("DEBUG"));
    TRACE      : constant Level := (intLevel => INT_TRACE, name => To_Unbounded_String("TRACE"));
    ALL_LEVELS : constant Level := (intLevel => INT_ALL,   name => To_Unbounded_String("ALL"));

end Logging.Level;

