#ifndef __DGEZGINHW3_H
#define __DGEZGINHW3_H

typedef enum { SetItem, SendItem, SchItem, RcpItem, MailItem } ItemType;
typedef enum { LIST, SET, SEND, SCH, RCP, MAIL } NodeType;

typedef struct identifierStruct {
        char * str;
        int line;
} identifierStruct;

struct DTnode {
    char *date;
    char *time;
    char *console;
    int isPrinted;
    struct DTnode *next;
};

typedef struct SetNode {
        char * identifier;
        char * value;
        int line;
} SetNode;

typedef struct SendNode {
        char * msg;
        int line;
        struct TreeNode * head;
        int isResolved;
        int scopeId;
} SendNode;

typedef struct SchNode {
        char * date;
        int dline;
        char * time;
        int tline;
        struct TreeNode * head;
        int isValidDate;
        int isValidTime;
        int isPrinted;
} SchNode;

typedef struct RcpNode {
        char * givenName;
        int line;
        int isResolved;
        char * address;
        int scopeId;
} RcpNode;

typedef struct MailNode {
        char * from;
        struct TreeNode * head;
        int scopeId;
} MailNode;

typedef struct ListNode {
        ItemType thisItemType;
        struct TreeNode * next;
        struct TreeNode * item;
} ListNode;

typedef union {
        SetNode set;
        SendNode send;
        SchNode sch;
        RcpNode rcp;
        MailNode mail;
        ListNode list;
} Node;


typedef struct TreeNode{
	NodeType thisNodeType;
        Node * nodePtr;
} TreeNode;

#endif