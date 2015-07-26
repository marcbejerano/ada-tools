with Logging.Logger;        use Logging.Logger;
with Logging.Appender;      use Logging.Appender;
with Logging.Level;         use Logging.Level;
with Logging.Event;         use Logging.Event;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Source_Info;

procedure LoggerTest is

    myLogger          : Logger_Ptr;
    myFileAppender    : Appender_Class_Ptr;
    myConsoleAppender : Appender_Class_Ptr;

begin
    Init_Logging("logger.properties");
    myLogger := Get_Logger("LoggerTest");
    myConsoleAppender := new Console_Appender'(Pattern => To_Unbounded_String("%d{ISO8601} [%-5p] %F:[%-4L] %m%n"));
    myFileAppender := new File_Appender'(
        Pattern => To_Unbounded_String("%d{ISO8601} [%-5p] %F:[%-4L] %m%n"),
        File_Name => To_Unbounded_String("test.log"));

    myLogger.Set_Level(TRACE);
    myLogger.Add_Appender(myConsoleAppender);
    myLogger.Add_Appender(myFileAppender);
    
    myLogger.Trace("TRACE woot!");
    myLogger.Debug("DEBUG woot!");
    myLogger.Info ("INFO  woot!");
    myLogger.Warn ("WARN  woot!");
    myLogger.Error("ERROR woot!");
    myLogger.Fatal("FATAL woot!");

    myLogger.Error(New_Log_Event("Error Message", GNAT.Source_Info.File, GNAT.Source_Info.Line));


    -- LoggerFactory
    -- Load from properties
end LoggerTest;

