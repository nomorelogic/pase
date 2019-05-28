# pase
Experimental case developed using free pascal and Lazarus


# About
This is an experiment on build up a general purpose c.a.s.e. [https://en.wikipedia.org/wiki/Computer-aided_software_engineering] 
useful to autogenerate (inheritable?) sources, technical and user documentation.
Currently focused in data modeling, in embrional and experimental state: is not ready for production but ready for study and code :)

# Graphic status
    Data Model (automatic history tracking) 
      |
      +- User data type 
         |
         +- Technical documentation (to do)
         |
         +- RDBMS Implementation
            |
            +- Firebird SQL (work in progress)

# To do
Improve nested templates
Improve runtime type information detection



# Example of use

Starting from a data model definition (eg. Employee)...

    procedure CreateDiagramEmployee(Analysis: TPaseModel);
    begin
      Analysis.Name:='Employee';
      Analysis.Caption:='Employee data model';
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

... and using this template...

    CREATE DOMAIN {+Name+}
           AS {+BaseType [-pftSizedString=VARCHAR({<Size>})-]
                         [-pftFixedString=CHAR({<Size>})-]
                         [-pftInteger=integer-]
                         [-pftIntegerSmall=smallint-]
                         [-pftFloat=DECIMAL({<Size>},{<Decimal>})-]
               +}
           {+DefaultValue[-Format=DEFAULT {<DefaultValue>}-]+}
           {+IsRequired[-True=NOT NULL-][-False=-]+}
           {+CheckValue+}
           {+Collate+}
    ;


... pase will generate this SQL script

    /*
     * =============================================
     * some info about this file
     * - - - - - - - - - - - - - - - - - - - - - - -
     * template: .../rdbms_firebird.sql
     * =============================================
    */


    /* 
     * - - - - - - - - - - - - - - - - - - - - - - -
     * user data type 
     * - - - - - - - - - - - - - - - - - - - - - - -
    */

    /* template: .../rdbms_firebird_datatype.sql */

    CREATE DOMAIN ADDRESSLINE
           AS VARCHAR(30)
           COLLATE NONE
    ;

    CREATE DOMAIN BUDGET
           AS DECIMAL(12,2)
           DEFAULT 50000
           CHECK (VALUE > 10000 AND VALUE <= 2000000)
    ;

    CREATE DOMAIN COUNTRYNAME
           AS VARCHAR(15)
           COLLATE NONE
    ;

    CREATE DOMAIN CUSTNO
           AS integer
           CHECK (VALUE > 1000)
    ;

    CREATE DOMAIN DEPTNO
           AS CHAR(3)
           CHECK (VALUE = '000' OR (VALUE > '0' AND VALUE <= '999') OR VALUE IS NULL)
           COLLATE NONE
    ;

    CREATE DOMAIN EMPNO
           AS smallint
    ;

    CREATE DOMAIN FIRSTNAME
           AS VARCHAR(15)
           COLLATE NONE
    ;

    CREATE DOMAIN JOBCODE
           AS VARCHAR(5)
           CHECK (VALUE > '99999')
           COLLATE NONE
    ;

    CREATE DOMAIN JOBGRADE
           AS smallint
           CHECK (VALUE BETWEEN 0 AND 6)
    ;

    CREATE DOMAIN LASTNAME
           AS VARCHAR(20)
           COLLATE NONE
    ;

    CREATE DOMAIN PHONENUMBER
           AS VARCHAR(20)
           COLLATE NONE
    ;

    CREATE DOMAIN PONUMBER
           AS CHAR(8)
           CHECK (VALUE STARTING WITH 'V')
           COLLATE NONE
    ;

    CREATE DOMAIN PRODTYPE
           AS VARCHAR(12)
           DEFAULT 'software'
           NOT NULL
           CHECK (VALUE IN ('software', 'hardware', 'other', 'N/A'))
           COLLATE NONE
    ;

    CREATE DOMAIN PROJNO
           AS CHAR(5)
           CHECK (VALUE = UPPER (VALUE))
           COLLATE NONE
    ;

    CREATE DOMAIN SALARY
           AS DECIMAL(10,2)
           DEFAULT 0
           CHECK (VALUE > 0)
    ;

in the same time, we can query the history of data model, checking out when an update was made...

    2017-06-29 09:12:5 Model      + TPaseModel    undefined             : undefined
    2017-06-29 09:12:5 DataType   + TPaseDataType Unknown               : pftUnknown
    2017-06-29 09:12:5 DataType   + TPaseDataType SizedString           : pftSizedString(-1)
    2017-06-29 09:12:5 DataType   + TPaseDataType FixedString           : pftFixedString
    2017-06-29 09:12:5 DataType   + TPaseDataType Integer               : pftInteger
    2017-06-29 09:12:5 DataType   + TPaseDataType Smallint              : pftIntegerSmall
    2017-06-29 09:12:5 DataType   + TPaseDataType Float                 : pftFloat(-1)
    2017-06-29 09:12:5 DataType   + TPaseDataType String                : pftString
    2017-06-29 09:12:5 DataType   + TPaseDataType Boolean               : pftBoolean
    2017-06-29 09:12:5 Model      e TPaseModel    Employee              : Renamed: Employee
    2017-06-29 09:12:5 Model      e TPaseModel    Release               : 0.1
    2017-06-29 09:12:5 DataType   + TPaseDataType ADDRESSLINE           : pftSizedString(30)
    2017-06-29 09:12:5 DataType   + TPaseDataType BUDGET                : pftFloat(12,2)
    2017-06-29 09:12:5 DataType   e TPaseDataType BUDGET                : DEFAULT: 50000
    2017-06-29 09:12:5 DataType   e TPaseDataType BUDGET                : CHECK (VALUE > 10000 AND VALUE <= 2000000)
    2017-06-29 09:12:5 DataType   + TPaseDataType COUNTRYNAME           : pftSizedString(15)
    2017-06-29 09:12:5 DataType   + TPaseDataType CUSTNO                : pftInteger
    2017-06-29 09:12:5 DataType   e TPaseDataType CUSTNO                : CHECK (VALUE > 1000)
    2017-06-29 09:12:5 DataType   + TPaseDataType DEPTNO                : pftFixedString
    2017-06-29 09:12:5 DataType   e TPaseDataType DEPTNO                : CHECK (VALUE = '000' OR (VALUE > '0' AND VALUE <= '999') OR VALUE IS NULL)
    2017-06-29 09:12:5 DataType   + TPaseDataType EMPNO                 : pftIntegerSmall
    2017-06-29 09:12:5 DataType   + TPaseDataType FIRSTNAME             : pftSizedString(15)
    2017-06-29 09:12:5 DataType   + TPaseDataType JOBCODE               : pftSizedString(5)
    2017-06-29 09:12:5 DataType   e TPaseDataType JOBCODE               : CHECK (VALUE > '99999')
    2017-06-29 09:12:5 DataType   + TPaseDataType JOBGRADE              : pftIntegerSmall
    2017-06-29 09:12:5 DataType   e TPaseDataType JOBGRADE              : CHECK (VALUE BETWEEN 0 AND 6)
    2017-06-29 09:12:5 DataType   + TPaseDataType LASTNAME              : pftSizedString(20)
    2017-06-29 09:12:5 DataType   + TPaseDataType PHONENUMBER           : pftSizedString(20)
    2017-06-29 09:12:5 DataType   + TPaseDataType PONUMBER              : pftFixedString
    2017-06-29 09:12:5 DataType   e TPaseDataType PONUMBER              : CHECK (VALUE STARTING WITH 'V')
    2017-06-29 09:12:5 DataType   + TPaseDataType PRODTYPE              : pftSizedString(12)
    2017-06-29 09:12:5 DataType   e TPaseDataType PRODTYPE              : CHECK (VALUE IN ('software', 'hardware', 'other', 'N/A'))
    2017-06-29 09:12:5 DataType   e TPaseDataType PRODTYPE              : DEFAULT: software
    2017-06-29 09:12:5 DataType   + TPaseDataType PROJNO                : pftFixedString
    2017-06-29 09:12:5 DataType   e TPaseDataType PROJNO                : CHECK (VALUE = UPPER (VALUE))
    2017-06-29 09:12:5 DataType   + TPaseDataType SALARY                : pftFloat(10,2)
    2017-06-29 09:12:5 DataType   e TPaseDataType SALARY                : DEFAULT: 0
    2017-06-29 09:12:5 DataType   e TPaseDataType SALARY                : CHECK (VALUE > 0)
    2017-06-29 09:12:5 Entity     + TPaseEntity   COUNTRY               : Caption=COUNTRY;
    2017-06-29 09:12:5 Field      + TPaseField    COUNTRY               : Type=COUNTRYNAME;Caption=Country name;
    2017-06-29 09:12:5 Field      + TPaseField    CURRENCY              : Type=SizedString[?];Caption=Currency name;
    2017-06-29 09:12:5 Index      + TPaseIndex    INTEG_2               : Primary Key: COUNTRY
