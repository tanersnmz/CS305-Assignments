%{
#include <stdio.h>
void yyerror (const char *s) /* Called by yyparse on error */
{
    return;
}
%}

%token tMAIL
%token tENDMAIL
%token tSCHEDULE
%token tENDSCHEDULE
%token tSEND
%token tSET
%token tTO
%token tFROM
%token tAT
%token tCOMMA
%token tCOLON
%token tLPR
%token tRPR
%token tLBR
%token tRBR
%token tIDENT
%token tSTRING
%token tADDRESS
%token tDATE
%token tTIME



%% 
program:

    |
    program component
    ;

component:
    mail_block
    |
    set_statement
    ;

mail_block:
    tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
    |
    tMAIL tFROM tADDRESS tCOLON tENDMAIL
    ;

set_statement:
    tSET tIDENT tLPR tSTRING tRPR
    ;

statement_list:
    statement
    |
    statement_list statement
    ;

statement: set_statement
    |
    send_statement
    |
    schedule_statement
    ;

send_statement:
    tSEND tLBR tIDENT tRBR tTO recipient_list
    | 
    tSEND tLBR tSTRING tRBR tTO recipient_list
    ;

schedule_statement:
    tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON send_list tENDSCHEDULE
    ;

send_list:
    send_statement
    |
    send_list send_statement
    ;

recipient_list: tLBR recipients tRBR
              ;

recipients: recipient
          | recipients tCOMMA recipient
          ;

recipient: tLPR tIDENT tCOMMA tADDRESS tRPR
         | tLPR tSTRING tCOMMA tADDRESS tRPR
         | tLPR tADDRESS tRPR
         ;





%%
int main ()
{
   if (yyparse()) {
        printf("ERROR\n");
        return 1;
    } else {
        printf("OK\n");
        return 0;
    }
}
