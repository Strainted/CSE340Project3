//LUKE ENGEL 


#ifndef __PARSER_H__
#define __PARSER_H__

#include <iostream>
#include <string>
#include <algorithm>
#include <vector>
#include <unordered_map>
#include "lexer.h"
#include "execute.h"


class parser {
    public:
        struct InstructionNode* parse_program(); 
        int location(std::string s);
        parser();
        
    private:
        LexicalAnalyzer lexer;
        std::unordered_map<std::string, int> locationTable;
        void parse_var_section();
        void parse_id_list();
        struct InstructionNode* parse_body();
        struct InstructionNode* parse_stmt_list();
        struct InstructionNode* parse_stmt();
        struct InstructionNode* parse_assignment_stmt();
        struct InstructionNode* parse_expr();
        Token parse_primary();
        Token parse_op();
        struct InstructionNode* parse_output_stmt();
        struct InstructionNode* parse_input_stmt();
        struct InstructionNode* parse_while_stmt();
        struct InstructionNode* parse_if_stmt();
        struct InstructionNode* parse_condition();
        Token parse_relop();
        struct InstructionNode* parse_switch_stmt();
        struct InstructionNode* parse_for_stmt();
        struct InstructionNode* parse_case_list(Token t, struct InstructionNode* node);
        struct InstructionNode* parse_case(Token t, struct InstructionNode* node);
        struct InstructionNode* parse_default_case();
        void parse_inputs();
        void parse_num_list();

        Token expect(TokenType t);

        struct InstructionNode* itterate(struct InstructionNode* p);
        struct InstructionNode* itterateTarget(struct InstructionNode* p);

};

#endif