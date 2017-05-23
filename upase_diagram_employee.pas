unit upase_diagram_employee;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , upaseglobals
  // , upasetypes
  ;

procedure CreateDiagramEmployee(Analysis: TPaseModel);


implementation

procedure CreateDiagramEmployee(Analysis: TPaseModel);
begin
  Analysis.Name:='Employee';
  Analysis.Caption:='Analisi analisi employee';
  Analysis.Release:='0.1';

  // - - - - - - - - - - - - - - - - - -
  // user data type
  // - - - - - - - - - - - - - - - - - -
  Analysis.AddUserType('ADDRESSLINE', 'SizedString', 30);
  Analysis.AddUserType('BUDGET', 'Float', 12, 2)
     .SetDefaultValue(50000)
     .SetCheckValue('CHECK (VALUE > 10000 AND VALUE <= 2000000)');
  Analysis.AddUserType('COUNTRYNAME', 'SizedString', 15);
  Analysis.AddUserType('CUSTNO', 'Integer')
     .SetCheckValue('CHECK (VALUE > 1000)');
  Analysis.AddUserType('DEPTNO', 'FixedString', 3)
     .SetCheckValue('CHECK (VALUE = ''000'' OR (VALUE > ''0'' AND VALUE <= ''999'') OR VALUE IS NULL)');
  Analysis.AddUserType('EMPNO', 'Smallint');
  Analysis.AddUserType('FIRSTNAME', 'SizedString', 15);
  Analysis.AddUserType('JOBCODE', 'SizedString', 5)
     .SetCheckValue('CHECK (VALUE > ''99999'')');
  Analysis.AddUserType('JOBGRADE', 'Smallint')
     .SetCheckValue('CHECK (VALUE BETWEEN 0 AND 6)');
  Analysis.AddUserType('LASTNAME', 'SizedString', 20);
  Analysis.AddUserType('PHONENUMBER', 'SizedString', 20);
  Analysis.AddUserType('PONUMBER', 'FixedString', 8)
     .SetCheckValue('CHECK (VALUE STARTING WITH ''V'')')
     .HasFixedSize:=TRUE;
  Analysis.AddUserType('PRODTYPE', 'SizedString', 12)
     .SetCheckValue('CHECK (VALUE IN (''software'', ''hardware'', ''other'', ''N/A''))')
     .SetDefaultValue('software')
     .SetRequired(True);
  Analysis.AddUserType('PROJNO', 'FixedString', 5)
     .SetCheckValue('CHECK (VALUE = UPPER (VALUE))');
  Analysis.AddUserType('SALARY', 'Float', 10, 2)
     .SetDefaultValue(0)
     .SetCheckValue('CHECK (VALUE > 0)');

end;

end.

