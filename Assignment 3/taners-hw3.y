%{
#ifdef YYDEBUG
  yydebug = 1;
#endif

#include "dgezgin-hw3.h"
#include <stdio.h>
#include <string.h>

void yyerror (const char *msg) /* Called by yyparse on error */ {return; }

TreeNode * mkLIST(TreeNode *next, TreeNode *item, ItemType type);
TreeNode * mkSET(char *name, char *value, int);
TreeNode * mkMAIL(TreeNode *head, char *address);
TreeNode * mkSCH(char *date, char *time, TreeNode *head, int, int);
TreeNode * mkSEND(char *value, TreeNode *head, int isResolved, int line);
TreeNode * mkRCP(char *value, char *address, int isResolved, int line);


char *getIdentValueWithName(char *, int, int);
void gIVWNRecursive(TreeNode * , char *, char **, int, int);
void resolveIdents(TreeNode* node);
void printSentNotifications(TreeNode *);
int isFirstInIndex(TreeNode *head, char* address, int seenIndex);
void printSentNotificationsMailBlock(TreeNode *head, char *from);
void printSentNotificationsSendBlock(TreeNode *node, char * from, char * msg, int, TreeNode*);

void printSentNotificationsSchBlock(TreeNode *node, char *from, int index, TreeNode *head, char *date, char *time);
void printSchNotificationsSendBlock(TreeNode *node, char *from, char *msg, int index, TreeNode *head, char *date, char *time, char **console);
struct DTnode* createDTnode(char *date, char *time, char *consoleValues, int isPrinted);
void sortLinkedList(struct DTnode* head);
void swapNodes(struct DTnode* node1, struct DTnode* node2);
char* createScheduledEmailString(char* from, char* date, char* time, char* to, char* msg);
void printLinkedList(struct DTnode* head);


TreeNode * rootPtr = NULL;
int currentScopeId = 0;
struct DTnode *headDT =NULL;
int isErrorFree = 1;

%}

%union {
  char *value;
  TreeNode *treePtr;
  identifierStruct identifierStruct;
}

%token <identifierStruct> tIDENT
%token <value> tSTRING
%token <value> tADDRESS
%token <identifierStruct> tDATE
%token <identifierStruct> tTIME

%token tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tSEND tTO tFROM tSET tCOMMA tCOLON tLPR tRPR tLBR tRBR tAT

%type <treePtr> statements
%type <treePtr> mailBlock
%type <treePtr> setStatement
%type <treePtr> statementList
%type <treePtr> sendStatements
%type <treePtr> sendStatement
%type <treePtr> scheduleStatement
%type <treePtr> recipientList
%type <treePtr> recipient


%start program

%%


program : statements {rootPtr = $1; }
;

statements :                                      {$$ = NULL;  }
            | setStatement statements             {$$ = mkLIST ( $2, $1, SetItem);   }
            | mailBlock statements                {$$ = mkLIST ( $2, $1, MailItem);  }
;

mailBlock : tMAIL tFROM tADDRESS tCOLON statementList tENDMAIL  {$$ = mkMAIL ($5, $3);  currentScopeId += 1;}
;

statementList :                                         {$$ = NULL;  }
                | setStatement statementList            {$$ = mkLIST ( $2, $1, SetItem);   }     
                | sendStatement statementList           {$$ = mkLIST ( $2, $1, SendItem);  }  
                | scheduleStatement statementList       {$$ = mkLIST ( $2, $1, SchItem);   }       
;

sendStatements : sendStatement                          {$$ = mkLIST ( NULL, $1, SendItem);   }
                | sendStatement sendStatements          {$$ = mkLIST ( $2, $1, SendItem);   }
;

sendStatement : tSEND tLBR tIDENT tRBR tTO tLBR recipientList tRBR     {$$ = mkSEND ( $3.str, $7, 0, $3.line);   }
              | tSEND tLBR tSTRING tRBR tTO tLBR recipientList tRBR    {$$ = mkSEND ( $3, $7, 1, -1);   }
;


recipientList : recipient                               {$$ = mkLIST ( NULL, $1, RcpItem);   }
            | recipient tCOMMA recipientList            {$$ = mkLIST ( $3, $1, RcpItem);   }
;

recipient : tLPR tADDRESS tRPR                          {$$ = mkRCP ( $2, $2, 1, -1);   }
            | tLPR tSTRING tCOMMA tADDRESS tRPR         {$$ = mkRCP ( $2, $4, 1, -1);   }
            | tLPR tIDENT tCOMMA tADDRESS tRPR          {$$ = mkRCP ( $2.str, $4, 0, $2.line);   }
;

scheduleStatement : tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON sendStatements tENDSCHEDULE {$$ = mkSCH ( $4.str, $6.str, $9, $4.line, $6.line); }
;

setStatement : tSET tIDENT tLPR tSTRING tRPR    {$$ = mkSET($2.str, $4, $2.line ); }
;


%%
int main () 
{
   if (yyparse())
   {
      printf("ERROR\n");
      return 1;
    } 
    else 
    {
      if (rootPtr == NULL){
        return 0;
      }
        resolveIdents(rootPtr);
        if(isErrorFree){
          printSentNotifications(rootPtr);
          sortLinkedList(headDT);
          printLinkedList(headDT);
        }
        return 0;
    } 
}

TreeNode * mkLIST ( TreeNode *next, TreeNode *item, ItemType type) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->nodePtr = (Node *)malloc (sizeof(Node));
  
  ret->thisNodeType = LIST;
  ret->nodePtr->list.thisItemType = type;
  ret->nodePtr->list.next = next;
  ret->nodePtr->list.item = item;
  return (ret);
}


TreeNode * mkSET (char * name, char * value, int line) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->thisNodeType = SET;
  ret->nodePtr = (Node *)malloc (sizeof(Node));

  ret->nodePtr->set.identifier = strdup(name);
  ret->nodePtr->set.value = strdup(value);
  ret->nodePtr->set.line = line;
  return (ret);
}

TreeNode * mkMAIL (TreeNode *head, char * address) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->thisNodeType = MAIL;
  ret->nodePtr = (Node *)malloc (sizeof(Node));

  ret->nodePtr->mail.head = head;
  ret->nodePtr->mail.from = strdup(address);
  ret->nodePtr->mail.scopeId = currentScopeId;
  return (ret);
}

TreeNode * mkSCH (char * date, char *time, TreeNode *head, int dline, int tline) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->thisNodeType = SCH;
  ret->nodePtr = (Node *)malloc (sizeof(Node));

  ret->nodePtr->sch.head = head;
  ret->nodePtr->sch.date = strdup(date);
  ret->nodePtr->sch.dline = dline;
  ret->nodePtr->sch.time = strdup(time);
  ret->nodePtr->sch.tline = tline;
  ret->nodePtr->sch.isValidDate = 0;
  ret->nodePtr->sch.isValidTime = 0;
  ret->nodePtr->sch.isPrinted = 0;
  return (ret);
}

TreeNode * mkSEND (char * value, TreeNode *head, int isResolved, int line) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->thisNodeType = SEND;
  ret->nodePtr = (Node *)malloc (sizeof(Node));

  ret->nodePtr->send.msg = strdup(value);
  ret->nodePtr->send.line = line;
  ret->nodePtr->send.head = head;
  ret->nodePtr->send.isResolved = isResolved;
  ret->nodePtr->send.scopeId = currentScopeId;
  return (ret);
}

TreeNode * mkRCP (char *value, char *address, int isResolved, int line) {
  TreeNode * ret = (TreeNode *)malloc (sizeof(TreeNode));
  ret->thisNodeType = RCP;
  ret->nodePtr = (Node *)malloc (sizeof(Node));

  ret->nodePtr->rcp.givenName = strdup(value);
  ret->nodePtr->rcp.line = line;
  ret->nodePtr->rcp.isResolved = isResolved;
  ret->nodePtr->rcp.address = strdup(address);
  ret->nodePtr->rcp.scopeId = currentScopeId;
  return (ret);
}

int isValidDate(char* date){
  int day, month, year;
  sscanf(date, "%d/%d/%d", &day, &month, &year);
  if (year < 0 || month < 1 || month > 12 || day < 1) {
    return 0;
  }
  int daysInMonth[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  if (month == 2 && (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)) {
    daysInMonth[2] = 29;
  }
  return day <= daysInMonth[month];
}

void resolveIdents(TreeNode* node){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == SendItem){
      if(!node->nodePtr->list.item->nodePtr->send.isResolved){
        char* value = getIdentValueWithName(node->nodePtr->list.item->nodePtr->send.msg, node->nodePtr->list.item->nodePtr->send.scopeId, node->nodePtr->list.item->nodePtr->send.line);
        if(value == NULL){
          isErrorFree = 0;
          printf("ERROR at line %d: %s is undefined\n", node->nodePtr->list.item->nodePtr->send.line, node->nodePtr->list.item->nodePtr->send.msg);
        }
        else{
          node->nodePtr->list.item->nodePtr->send.msg = strdup(value);
          node->nodePtr->list.item->nodePtr->send.isResolved = 1;
        }
      }
      if(node->nodePtr->list.item->nodePtr->send.head != NULL){
        resolveIdents(node->nodePtr->list.item->nodePtr->send.head);
      }
    }
    if(node->nodePtr->list.thisItemType == RcpItem){
      if(!node->nodePtr->list.item->nodePtr->rcp.isResolved){
        char* value = getIdentValueWithName(node->nodePtr->list.item->nodePtr->rcp.givenName, node->nodePtr->list.item->nodePtr->rcp.scopeId, node->nodePtr->list.item->nodePtr->rcp.line);
        if(value == NULL){
          isErrorFree = 0;
          printf("ERROR at line %d: %s is undefined\n", node->nodePtr->list.item->nodePtr->rcp.line, node->nodePtr->list.item->nodePtr->rcp.givenName);
        }
        else{
          node->nodePtr->list.item->nodePtr->rcp.givenName = strdup(value);
          node->nodePtr->list.item->nodePtr->rcp.isResolved = 1;
        }
      }
    }
    if(node->nodePtr->list.thisItemType == MailItem){
      if(node->nodePtr->list.item->nodePtr->mail.head != NULL){
        resolveIdents(node->nodePtr->list.item->nodePtr->mail.head);
      }
    }
    if(node->nodePtr->list.thisItemType == SchItem){
      if(isValidDate(node->nodePtr->list.item->nodePtr->sch.date)){
        node->nodePtr->list.item->nodePtr->sch.isValidDate = 1;
      }
      else{
        isErrorFree = 0;
        printf("ERROR at line %d: date object is not correct (%s)\n", node->nodePtr->list.item->nodePtr->sch.dline, node->nodePtr->list.item->nodePtr->sch.date);
      }
      int hour, minute;
      sscanf(node->nodePtr->list.item->nodePtr->sch.time, "%d:%d", &hour, &minute);
      if((hour >= 0 && hour <= 23) && (minute >= 0 && minute <= 59)){
        node->nodePtr->list.item->nodePtr->sch.isValidTime = 1;
      }
      else{
        isErrorFree = 0;
        printf("ERROR at line %d: time object is not correct (%s)\n", node->nodePtr->list.item->nodePtr->sch.tline, node->nodePtr->list.item->nodePtr->sch.time);
      }
      if(node->nodePtr->list.item->nodePtr->sch.head != NULL){
        resolveIdents(node->nodePtr->list.item->nodePtr->sch.head);
      }
    }
    if(node->nodePtr->list.next != NULL){
      resolveIdents(node->nodePtr->list.next);
    }
  }
  else{
    printf("ERROR FOR ME");
  }
}

char *getIdentValueWithName(char *name, int scopeId, int maxLine){
  char *latestValue = NULL;
  gIVWNRecursive(rootPtr, name, &latestValue, scopeId, maxLine); 
  return latestValue;
}

void gIVWNRecursive(TreeNode *node, char *name, char **latestValue, int scopeId, int maxLine){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == SetItem && node->nodePtr->list.item->nodePtr->set.line <= maxLine){
      //printf ("LOOKING: %s\n", node->nodePtr->list.item->nodePtr->set.identifier);
      if(strcmp(name, node->nodePtr->list.item->nodePtr->set.identifier) == 0){
        *latestValue = strdup(node->nodePtr->list.item->nodePtr->set.value); 
        //printf ("GOT: %s\n", *latestValue); 
      }
    }
    if(node->nodePtr->list.next != NULL){
      gIVWNRecursive(node->nodePtr->list.next, name, latestValue, scopeId, maxLine);
    }
    if(node->nodePtr->list.thisItemType == MailItem){
      if(node->nodePtr->list.item->nodePtr->mail.head != NULL && node->nodePtr->list.item->nodePtr->mail.scopeId == scopeId){
        gIVWNRecursive(node->nodePtr->list.item->nodePtr->mail.head, name, latestValue, scopeId, maxLine);
      }
    }
  }
  else{
    printf("WARNING FOR ME");    
  }
}

void printSentNotifications(TreeNode *node){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == MailItem){
      printSentNotificationsMailBlock(node->nodePtr->list.item->nodePtr->mail.head, node->nodePtr->list.item->nodePtr->mail.from);
    }
    if(node->nodePtr->list.next != NULL){
      printSentNotifications(node->nodePtr->list.next);
    }
  }
}

int isFirstInIndex(TreeNode *head, char* address, int seenIndex){
  int index = 0;
  TreeNode* curr = head;
  while(curr != NULL){
      if(strcmp(curr->nodePtr->list.item->nodePtr->rcp.address, address) == 0){
        return (seenIndex == index);
    }
    curr = curr->nodePtr->list.next;
    index += 1;
  }
  printf("ERROR FOR ME");
  return 0;
}

void printSentNotificationsMailBlock(TreeNode *node, char * from){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == SendItem){
      printSentNotificationsSendBlock(node->nodePtr->list.item->nodePtr->send.head, from, node->nodePtr->list.item->nodePtr->send.msg, 0, node->nodePtr->list.item->nodePtr->send.head);
    }
    if(node->nodePtr->list.thisItemType == SchItem){
      printSentNotificationsSchBlock(node->nodePtr->list.item->nodePtr->sch.head, from, 0, node->nodePtr->list.item->nodePtr->sch.head, node->nodePtr->list.item->nodePtr->sch.date, node->nodePtr->list.item->nodePtr->sch.time);
    }
    if(node->nodePtr->list.next != NULL){
      printSentNotificationsMailBlock(node->nodePtr->list.next, from);
    }
  }
}

void printSentNotificationsSchBlock(TreeNode *node, char *from, int index, TreeNode *head, char *date, char *time){
    if(node->thisNodeType == LIST){
      char *console = (char *)malloc(100);
      strcpy(console, "");
        if(node->nodePtr->list.thisItemType == SendItem){ 
            printSchNotificationsSendBlock(node->nodePtr->list.item->nodePtr->send.head, from, node->nodePtr->list.item->nodePtr->send.msg, 0, node->nodePtr->list.item->nodePtr->send.head, date, time, &console);
            struct DTnode *temp = createDTnode(date, time, console, 0);
            temp->next = headDT;
            headDT = temp;
        }
        if(node->nodePtr->list.next != NULL){
            index += 1;
            printSentNotificationsSchBlock(node->nodePtr->list.next, from, index, head, date, time);
        }
    }
}

void printSentNotificationsSendBlock(TreeNode *node, char * from, char * msg, int index, TreeNode *head){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == RcpItem){
      if(isFirstInIndex(head, node->nodePtr->list.item->nodePtr->rcp.address, index)) printf("E-mail sent from %s to %s: \"%s\"\n", from, node->nodePtr->list.item->nodePtr->rcp.givenName ,msg);
    }
    if(node->nodePtr->list.next != NULL){
      index += 1;
      printSentNotificationsSendBlock(node->nodePtr->list.next, from, msg, index, head);
    }
  }
}

void printSchNotificationsSendBlock(TreeNode *node, char * from, char * msg, int index, TreeNode *head, char * date, char * time, char **console){
  if(node->thisNodeType == LIST){
    if(node->nodePtr->list.thisItemType == RcpItem){
      if(isFirstInIndex(head, node->nodePtr->list.item->nodePtr->rcp.address, index)) {
        char *consoleNext = createScheduledEmailString(from,date, time, node->nodePtr->list.item->nodePtr->rcp.givenName, msg);
        if (strlen(*console) == 0) {
          strcpy(*console, consoleNext);
        } else {
          strcat(*console, "\n");
          strcat(*console, consoleNext);
        }
      }
    }
    if(node->nodePtr->list.next != NULL){
      index += 1;
      printSchNotificationsSendBlock(node->nodePtr->list.next, from, msg, index, head, date, time, console);
    }
  }
}
struct DTnode* createDTnode(char *date, char *time, char *consoleValues, int isPrinted) {
    struct DTnode *newDTnode = (struct DTnode*)malloc(sizeof(struct DTnode));
    
    // Allocate memory and copy strings
    newDTnode->date = strdup(date);
    newDTnode->time = strdup(time);
    newDTnode->console = strdup(consoleValues);
    
    newDTnode->isPrinted = isPrinted;
    newDTnode->next = NULL;
    
    return newDTnode;
}

void sortLinkedList(struct DTnode* head) {
    int swapped;
    struct DTnode *ptr1;
    struct DTnode *lptr = NULL;

    // Checking for empty list
    if (head == NULL)
        return;

    do {
        swapped = 0;
        ptr1 = head;

        while (ptr1->next != lptr) {
            // Compare current and next nodes using the helper function
            if (isFirstEarlierThanSecond(ptr1->next->date, ptr1->next->time, ptr1->date, ptr1->time)) {
                // Swap nodes if the next node is earlier
                swapNodes(ptr1, ptr1->next);
                swapped = 1;
            }
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    } while (swapped);
}

void swapNodes(struct DTnode* node1, struct DTnode* node2) {
    char *tempDate, *tempTime, *tempConsole;
    int tempIsPrinted;

    // Swap date, time, and console values
    tempDate = node1->date;
    tempTime = node1->time;
    tempConsole = node1->console;
    tempIsPrinted = node1->isPrinted;

    node1->date = node2->date;
    node1->time = node2->time;
    node1->console = node2->console;
    node1->isPrinted = node2->isPrinted;

    node2->date = tempDate;
    node2->time = tempTime;
    node2->console = tempConsole;
    node2->isPrinted = tempIsPrinted;
}

int isFirstEarlierThanSecond(char* tDate1, char* tTime1, char* tDate2, char* tTime2) {
    // Parse date and time components
    int day1, month1, year1, hour1, minute1;
    sscanf(tDate1, "%d/%d/%d", &day1, &month1, &year1);
    sscanf(tTime1, "%d:%d", &hour1, &minute1);

    int day2, month2, year2, hour2, minute2;
    sscanf(tDate2, "%d/%d/%d", &day2, &month2, &year2);
    sscanf(tTime2, "%d:%d", &hour2, &minute2);

    // Compare the years
    if (year1 < year2) {
        return 1;  // First date is earlier
    } else if (year1 > year2) {
        return 0;  // Second date is earlier
    }

    // Compare the months
    if (month1 < month2) {
        return 1;
    } else if (month1 > month2) {
        return 0;
    }

    // Compare the days
    if (day1 < day2) {
        return 1;
    } else if (day1 > day2) {
        return 0;
    }

    // Compare the hours
    if (hour1 < hour2) {
        return 1;
    } else if (hour1 > hour2) {
        return 0;
    }

    // Compare the minutes
    return (minute1 <= minute2);
}

void printLinkedList(struct DTnode* head) {
    struct DTnode* current = head;
    while (current != NULL) {
        printf("%s\n", current->console);
        current = current->next;
    }
}

char* createScheduledEmailString(char* from, char* date, char* time, char* to, char* msg) {
  int day, month, year;
  sscanf(date, "%d/%d/%d", &day, &month, &year);
  const char* monthNames[] = {"", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
  char* convertedDate = (char*)malloc(50);
  sprintf(convertedDate, "%s %d, %d", monthNames[month], day, year);
      
  int length = snprintf(NULL, 0, "E-mail is scheduled to be sent from %s on %s, %s to %s: \"%s\"",
                       from, convertedDate, time, to, msg);
  char* scheduledEmailString = (char*)malloc(length + 1);
  sprintf(scheduledEmailString, "E-mail is scheduled to be sent from %s on %s, %s to %s: \"%s\"",
        from, convertedDate, time, to, msg);

  return scheduledEmailString;
}