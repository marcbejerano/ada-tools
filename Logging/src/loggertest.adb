with Logging.Logger; use Logging.Logger;
with Logging.Level;  use Logging.Level;

procedure LoggerTest is

    myLogger : Logger_Ptr := Get_Logger("LoggerTest");

begin
    myLogger.Set_Level(INFO);
    --myLogger.Get_Handlers()
    myLogger.Set_Pattern("%d{ISO8601} %-5p [%t]: %m%n");
    
    myLogger.error("woot!");
end LoggerTest;

