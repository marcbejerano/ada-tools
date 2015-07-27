with Logging.Logger;        use Logging.Logger;
with Logging.Level;         use Logging.Level;
with Logging.Event;         use Logging.Event;
with GNAT.Source_Info;

procedure LoggerTest is

    myLogger          : Logger_Ptr;

begin
    Init_Logging("logger.properties");
    myLogger := Get_Logger("LoggerTest");

    myLogger.Set_Level(TRACE);
    
    myLogger.Trace("TRACE woot!");
    myLogger.Debug("DEBUG woot!");
    myLogger.Info ("INFO  woot!");
    myLogger.Warn ("WARN  woot!");
    myLogger.Error("ERROR woot!");
    myLogger.Fatal("FATAL woot!");

    myLogger.Error(New_Log_Event("Error Message", GNAT.Source_Info.File, GNAT.Source_Info.Line));
end LoggerTest;

