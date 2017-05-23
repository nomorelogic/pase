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
