/* sas_test.sas */
/* Define a data step to create a dataset */
data work.testData;
    input name $;
    datalines;
    World
    ;
run;

/* Define a macro */
%macro sayHello(name);
    %put Hello, &name.;
%mend sayHello;

/* Call the macro */
%sayHello(World);

/* Define a PROC SQL to create a table */
PROC SQL;
    CREATE TABLE work.testTable AS
    SELECT * FROM work.testData;
QUIT;
