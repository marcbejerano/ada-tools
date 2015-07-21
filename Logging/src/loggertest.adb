with Logging.Logger;        use Logging.Logger;
with Logging.Appender;      use Logging.Appender;
with Logging.Level;         use Logging.Level;
with Logging.Event;         use Logging.Event;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Source_Info;

procedure LoggerTest is

    myLogger : Logger_Ptr := Get_Logger("LoggerTest");

begin
    --myLog.File_Name := To_Unbounded_String("loggertest-appenders.log");

    myLogger.Set_Level(TRACE);
    --myLogger.Get_Handlers()
    myLogger.Add_Appender(new Console_Appender);
    myLogger.Add_Appender(new File_Appender'(File_Name => To_Unbounded_String("test.log")));
    
    myLogger.Trace("TRACE woot!");
    myLogger.Debug("DEBUG woot!");
    myLogger.Info ("INFO  woot!");
    myLogger.Warn ("WARN  woot!");
    myLogger.Error("ERROR woot!");
    myLogger.Fatal("FATAL woot!");

    myLogger.Set_Pattern("%d{ISO8601} [%-5p] %F:[%-4L] %m%n");
    myLogger.Error(New_Log_Event("Error Message", GNAT.Source_Info.File, GNAT.Source_Info.Line));

end LoggerTest;

