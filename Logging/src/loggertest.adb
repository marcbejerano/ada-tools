with Logging.Logger; use Logging.Logger;
with Logging.Level;  use Logging.Level;

procedure LoggerTest is

    myLogger : Logger_Ptr := Get_Logger("LoggerTest");
    myEvent  : Log_Event;

begin
    myLogger.Set_Level(INFO);
    --myLogger.Get_Handlers()
    myLogger.Set_Pattern("%d{ISO8601} [%-5p] %F:%-4L %m%n");
    
    myLogger.trace("TRACE woot!");
    myLogger.debug("DEBUG woot!");
    myLogger.info ("INFO  woot!");
    myLogger.warn ("WARN  woot!");
    myLogger.error("ERROR woot!");
    myLogger.fatal("FATAL woot!");

    myEvent := New_Log_Event("This is a message", "loggertest.adb", 20);

    myLogger.error(myEvent);
end LoggerTest;

