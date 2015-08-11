with Ada.Text_IO; use Ada.Text_IO;

procedure TestCSV is

    csv: CSV_File;

begin
    Open(csv, "FL_insurance_sample.csv", has_header => true);

    Close(csv);
end TestCSV;

