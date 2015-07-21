with Logging.Logger;   use Logging.Logger;
with Logging.Level;    use Logging.Level;
with GNAT.Source_Info;

procedure LoggerTest is

    myLogger : Logger_Ptr := Get_Logger("LoggerTest");

begin
    myLogger.Set_Level(TRACE);
    --myLogger.Get_Handlers()
    
    myLogger.trace("TRACE woot!");
    myLogger.debug("DEBUG woot!");
    myLogger.info ("INFO  woot!");
    myLogger.warn ("WARN  woot!");
    myLogger.error("ERROR woot!");
    myLogger.fatal("FATAL woot!");

    myLogger.Set_Pattern("%d{ISO8601} [%-5p] %F:[%-4L] %m%n");
    myLogger.error(New_Log_Event("Error Message", GNAT.Source_Info.File, GNAT.Source_Info.Line));
end LoggerTest;

