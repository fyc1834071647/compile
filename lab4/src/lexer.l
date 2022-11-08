%option noyywrap
/* 1.声明 */
/* c/c++代码，被原样复制到c/cpp文件中 */
%{
    /*
    * You will need to comment this line in lab5.
    */
    #define ONLY_FOR_LEX
    
    #ifdef ONLY_FOR_LEX
    #else
    #define YYSTYPE void *
    #include "parser.h"
    #endif

    #define YY_NO_UNPUT
    #define YY_NO_INPUT
    #include <string>
    #include <vector>
    #ifdef ONLY_FOR_LEX
    #include <ostream>
    #include <fstream>
    #include <iostream>
    #include <math.h>
    using namespace std;
    extern FILE *yyin; 
    extern FILE *yyout;

    void DEBUG_FOR_LAB4(std::string s){
        std::string DEBUG_INFO = "[DEBUG LAB4]: \t\t" + s + "\n";
        fputs(DEBUG_INFO.c_str(), yyout);
    }
    #endif
%}

/* 2.定义 */
/* 定义一些正则表达式的名字，在规则中使用 */
ID [[:alpha:]_][[:alpha:][:digit:]_]*
DECIMAL ([1-9][0-9]*|0)
EOL (\r\n|\n|\r)
WHITE [\t\t ]

linecomment (\/\/.*|\/\/.*)
commentbegin "/*"
commentelement .|\n
commentend "*/"
%x BLOCKCOMMENT

/* 3.规则 */
/* 由匹配模式和事件组成 */
%%
{linecomment} {
    #ifdef ONLY_FOR_LEX
        // 啥都不做
    #else 
        return linecomment;
    #endif
}

{commentbegin} {
    #ifdef ONLY_FOR_LEX
        BEGIN BLOCKCOMMENT;
        string a = yytext;
        while(a.find('\n') != a.npos)
        {
            yylineno++;
            a.replace(a.find('\n'),1,"");
        }
    #else 
        return commentbegin;
    #endif
}

<BLOCKCOMMENT>{commentend} {
    #ifdef ONLY_FOR_LEX
        string a = yytext;
        if(a.find("\n") != a.npos)
        {
            yylineno++;
            a.replace(a.find("\n"),1,"");
        }
        BEGIN INITIAL;
    #else 
        return commentend;
    #endif
}

<BLOCKCOMMENT>{commentelement} {
    #ifdef ONLY_FOR_LEX
        string a = yytext;
        if(a.find('\n') != a.npos)
        {
            yylineno++;
            a.replace(a.find('\n'),1,"");
        }
    #else 
        return commentelement;
    #endif
}



"int" {
    /*
    * Questions: 
    *   Q1: Why we need to return INT in further labs?
    *   Q2: What is "INT" actually?
    */
    #ifdef ONLY_FOR_LEX
        string temp = "INT\t\tint\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return INT;
    #endif
}
"void" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("VOID\tvoid");
    #else
        return VOID;
    #endif 
}
"if" {
    #ifdef ONLY_FOR_LEX
        string temp = "IF\t\tif\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return IF;
    #endif
};
"else" {
    #ifdef ONLY_FOR_LEX
        string temp = "ELSE\t\telse\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return ELSE;
    #endif
};
"return" {
    #ifdef ONLY_FOR_LEX
        string temp = "RETURN\t\treturn\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return RETURN;
    #endif
}
"==" {
    #ifdef ONLY_FOR_LEX
        string temp = "EQUAL\t\t==\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return EQUAL;
    #endif
}
"=" {
    #ifdef ONLY_FOR_LEX
        string temp = "ASSIGN\t\t=\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return ASSIGN;
    #endif
}
"!=" {
    #ifdef ONLY_FOR_LEX
        string temp = "UNEQUAL\t\t!=\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return UNEQUAL;
    #endif
}
"!" {
    #ifdef ONLY_FOR_LEX
        string temp = "AGAINST\t\t!\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return AGAINST;
    #endif
}
"*" {
    #ifdef ONLY_FOR_LEX
        string temp = "STAR\t\t*\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return STAR;
    #endif
}
">=" {
    #ifdef ONLY_FOR_LEX
        string temp = "GTE\t\t>=\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return GTE;
    #endif
}
"<=" {
    #ifdef ONLY_FOR_LEX
        string temp = "LTE\t\t<=\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return LTE;
    #endif
}
"<" {
    #ifdef ONLY_FOR_LEX
        string temp = "LESS\t\t<\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return LESS;
    #endif
}
">" {
    #ifdef ONLY_FOR_LEX
        string temp = "MORE\t\t>\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return MORE;
    #endif
}
"+" {
    #ifdef ONLY_FOR_LEX
        string temp = "ADD\t\t+\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return ADD;
    #endif
}
"-" {
    #ifdef ONLY_FOR_LEX
        string temp = "SUB\t\t-\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return SUB;
    #endif
}
"/" {
    #ifdef ONLY_FOR_LEX
        string temp = "DIV\t\t/\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return DIV;
    #endif
}
"%" {
    #ifdef ONLY_FOR_LEX
        string temp = "MOD\t\t%\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return MOD;
    #endif
}
";" {
    #ifdef ONLY_FOR_LEX
        string temp = "SEMICOLON\t\t;\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return SEMICOLON;
    #endif
}
"(" {
    #ifdef ONLY_FOR_LEX
        string temp = "LPAREN\t\t(\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else 
        return LPAREN; 
    #endif
}
")" {
    #ifdef ONLY_FOR_LEX
        string temp = "RPAREN\t\t)\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return RPAREN;
    #endif
}
"{" {
    #ifdef ONLY_FOR_LEX
        string temp = "LBRACE\t\t{\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return LBRACE;
    #endif
}
"}" {
    #ifdef ONLY_FOR_LEX
        string temp = "RBRACE\t\t}\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return RBRACE;
    #endif
}
"[" {
    #ifdef ONLY_FOR_LEX
        string temp = "LSQUARE\t\t[\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return LSQUARE;
    #endif
}
"]" {
    #ifdef ONLY_FOR_LEX
        string temp = "RSQUARE\t\t]\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return RSQUARE;
    #endif
}

"while" {
    #ifdef ONLY_FOR_LEX
        string temp = "WHILE\t\twhile\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else
        return WHILE;
    #endif
}
{ID} {
    #ifdef ONLY_FOR_LEX
        string m = yytext;
        string temp = "ID\t\t" + m + "\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else 
        return ID;
    #endif
}
{DECIMAL} {
    #ifdef ONLY_FOR_LEX
        string a = yytext;
        string temp = "DECIMIAL\t\t" + a + "\t\tline:" + to_string(yylineno);
        DEBUG_FOR_LAB4(temp);
    #else 
        return DECIMIAL;
    #endif
}

{EOL} yylineno++;
{WHITE}
%%

/* 4.用户定义过程 */
#ifdef ONLY_FOR_LEX
int main(int argc, char **argv){
    if(argc != 5){
        fprintf(stderr, "Argument Not Enough");
        exit(EXIT_FAILURE);
    }

    if(!(yyin = fopen(argv[1], "r"))){
        fprintf(stderr, "No such file or directory: %s", argv[1]);
        exit(EXIT_FAILURE);
    }

    if(!(yyout = fopen(argv[3], "w"))){
        fprintf(stderr, "No such file or directory: %s", argv[3]);
        exit(EXIT_FAILURE);
    }
    yylex();
    return 0;
}
#endif