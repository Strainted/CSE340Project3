//LUKE ENGEL 

#include "parser.h"
using namespace std;


parser::parser() : lexer() {

}

Token parser::expect(TokenType t) {
    Token t1 = lexer.GetToken();
    if (t1.token_type == t) {
        return t1;
    }
    else {
        exit(1);
    }
}

int parser::location(std::string s) {
    return locationTable[s];
}

struct InstructionNode* parser::parse_program() {
    parse_var_section();
    struct InstructionNode* newNode = parse_body();//This should contain the linked list to be executed
    
    parse_inputs();
    return newNode;
    //send the linked list to the exectuion 
}

void parser::parse_var_section() {
    parse_id_list();
    expect(SEMICOLON);
}

void parser::parse_id_list() {
    Token t1 = expect(ID);

    int memLocation = next_available; //allocate "memory" for variables
    locationTable[t1.lexeme] = memLocation;
    mem[memLocation] = 0;
    next_available++;

    Token t2 = lexer.peek(1);
    if (t2.token_type == COMMA) {
        expect(COMMA);
        parse_id_list();
    }

}

struct InstructionNode* parser::parse_body() {
    struct InstructionNode* instList;
    expect(LBRACE);
    instList = parse_stmt_list();
    expect(RBRACE);
    return instList;
}

struct InstructionNode* parser::parse_stmt_list() {
    struct InstructionNode* inst = nullptr;
    struct InstructionNode* instList = nullptr;
    inst = parse_stmt();
    Token t1 = lexer.peek(1);
    if (t1.token_type == ID || t1.token_type == WHILE || t1.token_type == IF || 
        t1.token_type == SWITCH || t1.token_type == FOR || t1.token_type == OUTPUT || t1.token_type == INPUT) {
            instList = parse_stmt_list();
            itterate(inst)->next = instList; //TODO DOUBLE CHECK THIS IS WHERE YOU ITTERATE TO THE END
    }
    return inst;
}

//TODO make helper function to itterate to the end of instlist itterate through instruction nodes
struct InstructionNode* parser::itterate(struct InstructionNode* p) {
    while (p->next != nullptr) {
        p = p->next;
    }
    return p;
}

struct InstructionNode* parser::parse_stmt() {
    Token t1 = lexer.peek(1);
    struct InstructionNode* newNode = nullptr;
    switch(t1.token_type) {
        case ID: 
            newNode = parse_assignment_stmt();
            break;

        case WHILE:
            newNode = parse_while_stmt();
            break;

        case IF:
            newNode = parse_if_stmt();
            break;

        case SWITCH:
            newNode = parse_switch_stmt();
            break;
                              
        case FOR:
            newNode = parse_for_stmt();
            break;

        case OUTPUT:
            newNode = parse_output_stmt();
            break;

        case INPUT:
            newNode = parse_input_stmt();
            break;
    }

    return newNode;
}

struct InstructionNode* parser::parse_assignment_stmt() {
    Token t1 = expect(ID);
    expect(EQUAL);
    Token t3 = lexer.peek(1);
    Token t4 = lexer.peek(2);
    struct InstructionNode* newNode = nullptr;
    if ((t3.token_type == ID || t3.token_type == NUM) && (t4.token_type == SEMICOLON)) { 
        Token p = parse_primary();

        newNode = new InstructionNode();
        newNode->type = ASSIGN;
        newNode->assign_inst.left_hand_side_index = locationTable[t1.lexeme];
        newNode->assign_inst.op = OPERATOR_NONE;
        newNode->assign_inst.opernd1_index = locationTable[p.lexeme];
        newNode->assign_inst.opernd2_index = -1;
        newNode->next = nullptr;
        
    }
    else if ((t3.token_type == ID || t3.token_type == NUM) && (t4.token_type == PLUS || t4.token_type == MINUS || t4.token_type == MULT || t4.token_type == DIV)) {
        newNode = parse_expr();
        newNode->type = ASSIGN;
        newNode->assign_inst.left_hand_side_index = locationTable[t1.lexeme];
        newNode->next = nullptr;

    }
    expect(SEMICOLON);

    return newNode;
    
}

struct InstructionNode* parser::parse_expr() {
    Token p1 = parse_primary();
    Token o = parse_op();
    Token p2 = parse_primary();

    struct InstructionNode* newNode = new InstructionNode();
    newNode->assign_inst.opernd1_index = locationTable[p1.lexeme];
    newNode->assign_inst.opernd2_index = locationTable[p2.lexeme];
    if (o.token_type == PLUS) {
        newNode->assign_inst.op = OPERATOR_PLUS;
    }
    else if (o.token_type == MINUS) {
        newNode->assign_inst.op = OPERATOR_MINUS;
    }
    else if (o.token_type == MULT) {
        newNode->assign_inst.op = OPERATOR_MULT;
    }
    else if (o.token_type == DIV) {
        newNode->assign_inst.op = OPERATOR_DIV;
    }
    
    return newNode;
}

Token parser::parse_primary() {
    Token t1 = lexer.peek(1);
    if (t1.token_type == ID) {
        return expect(ID); //should already have mem allocated
        
    }
    else {
        Token t3 = expect(NUM);
        auto it = locationTable.find(t3.lexeme);
        if (it == locationTable.end()) {
            int memLocation = next_available; //allocate "memory" for constants 
            locationTable[t3.lexeme] = memLocation;
            mem[memLocation] = std::stoi(t3.lexeme); //using stoi to convert NUM string to int Ex. "4" = 4
            next_available++;
        }
        
        return t3;
    }
}

Token parser::parse_op() {
    Token t1 = lexer.peek(1);
    if (t1.token_type == PLUS) {
        return expect(PLUS);
    }
    else if (t1.token_type == MINUS) {
        return expect(MINUS);
    }
    else if (t1.token_type == MULT) {
        return expect(MULT);
    }
    else {
        return expect(DIV);
    }
}

struct InstructionNode* parser::parse_output_stmt() {
    expect(OUTPUT);
    Token t = expect(ID);
    expect(SEMICOLON);

    struct InstructionNode* outputNode = new InstructionNode();
    outputNode->type = OUT;
    outputNode->output_inst.var_index = location(t.lexeme);
    outputNode->next = nullptr;
    return outputNode;
}

struct InstructionNode* parser::parse_input_stmt() {
    expect(INPUT);
    Token t = expect(ID);
    expect(SEMICOLON);

    struct InstructionNode* inputNode = new InstructionNode();
    inputNode->type = IN;
    inputNode->input_inst.var_index = location(t.lexeme);
    inputNode->next = nullptr;
    return inputNode;
}

struct InstructionNode* parser::parse_while_stmt() {
    expect(WHILE);
    struct InstructionNode *whileNode;
    whileNode = parse_condition();

    whileNode->type = CJMP;
    whileNode->next = parse_body();

    struct InstructionNode* jumpNode = new InstructionNode();
    jumpNode->type = JMP;
    jumpNode->jmp_inst.target = whileNode;

    itterate(whileNode)->next = jumpNode;

    struct InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    noopNode->next = nullptr;
    jumpNode->next  = noopNode;

    whileNode->cjmp_inst.target = noopNode;

    return whileNode;

}

struct InstructionNode* parser::parse_if_stmt() {
    expect(IF);
    struct InstructionNode* inst;
    inst = parse_condition();
    inst->type = CJMP;
    inst->next = parse_body(); 

    struct InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    noopNode->next = nullptr;

    struct InstructionNode* p = inst;
    while (p->next != nullptr) {
        p = p->next;
    }
    p->next = noopNode;
    inst->cjmp_inst.target = noopNode;

    return inst;
}

struct InstructionNode* parser::parse_condition() {
    Token p1 = parse_primary();
    Token relop = parse_relop();
    Token p2 = parse_primary();

    struct InstructionNode* conditionNode = new InstructionNode();
    conditionNode->cjmp_inst.opernd1_index = location(p1.lexeme);
    conditionNode->cjmp_inst.opernd2_index = location(p2.lexeme);
    if (relop.token_type == GREATER) {
        conditionNode->cjmp_inst.condition_op = CONDITION_GREATER;
    }
    else if (relop.token_type == LESS) {
        conditionNode->cjmp_inst.condition_op = CONDITION_LESS;
    }
    else if (relop.token_type == NOTEQUAL) {
        conditionNode->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
    }
    conditionNode->next = nullptr;
    return conditionNode;
}

Token parser::parse_relop() {
    Token t1 = lexer.peek(1);
    if (t1.token_type == GREATER) {
        return expect(GREATER);
    }
    else if (t1.token_type == LESS) {
        return expect(LESS);
    }
    else {
        return expect(NOTEQUAL);
    }
}

struct InstructionNode* parser::itterateTarget(struct InstructionNode* p) {
    while (p->cjmp_inst.target != nullptr) {
        p = p->cjmp_inst.target;
    }
    return p;
}

struct InstructionNode* parser::parse_switch_stmt() {
    struct InstructionNode* switchNode;
    struct InstructionNode* defaultNode;
    struct InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    noopNode->next = nullptr;
    expect(SWITCH);
    Token t = expect(ID);
    expect(LBRACE);
    switchNode = parse_case_list(t, noopNode);
    Token t1 = lexer.peek(1);
    if (t1.token_type == DEFAULT) {
        defaultNode = parse_default_case();
        itterate(defaultNode)->next = noopNode;
        itterate(switchNode)->next = defaultNode;
    }
    else {
        itterate(switchNode)->next = noopNode;
    }
    
    expect(RBRACE);
    return switchNode;
}

struct InstructionNode* parser::parse_for_stmt() {
    struct InstructionNode* forNode;
    struct InstructionNode* conditionNode;
    struct InstructionNode* assign2Node;
    expect(FOR);
    expect(LPAREN);
    forNode = parse_assignment_stmt();
    forNode->type = ASSIGN;
    conditionNode = parse_condition();
    forNode->next = conditionNode;
    conditionNode->type = CJMP; 
    expect(SEMICOLON);
    assign2Node = parse_assignment_stmt();
    assign2Node->type = ASSIGN;
    assign2Node->next = nullptr;
    expect(RPAREN);
    conditionNode->next = parse_body();
    itterate(conditionNode)->next = assign2Node;

    struct InstructionNode* jumpNode = new InstructionNode();
    assign2Node->next = jumpNode;
    jumpNode->type = JMP;
    jumpNode->jmp_inst.target = conditionNode; //CJMP node pointer
    jumpNode->next = nullptr;

    itterate(assign2Node)->next = jumpNode;

    struct InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    noopNode->next = nullptr;
    jumpNode->next = noopNode;

    conditionNode->cjmp_inst.target = noopNode;

    return forNode;

}

struct InstructionNode* parser::parse_case_list(Token t1, struct InstructionNode* noopNode) {
    struct InstructionNode* caseNode;
    struct InstructionNode* caseList;
    caseNode = parse_case(t1, noopNode);
    
    Token t2 = lexer.peek(1);
    if (t2.token_type == CASE) {
        caseList = parse_case_list(t1, noopNode);
        itterate(caseNode)->next = caseList;
    }

    return caseNode;
}

struct InstructionNode* parser::parse_case(Token t1, struct InstructionNode* noopNode) {
    struct InstructionNode* caseBody;
    struct InstructionNode* caseNode;
    expect(CASE);
    Token t2 = parse_primary();
    expect(COLON);
    caseNode = new InstructionNode();
    caseNode->type = CJMP;
    caseNode->cjmp_inst.opernd1_index = location(t1.lexeme);
    caseNode->cjmp_inst.opernd2_index = location(t2.lexeme);
    caseNode->cjmp_inst.condition_op = CONDITION_NOTEQUAL; 
    caseBody = parse_body();
    caseNode->cjmp_inst.target = caseBody;
   
    struct InstructionNode* jumpNode = new InstructionNode();
    jumpNode->type = JMP;
    jumpNode->jmp_inst.target = noopNode;
    itterate(caseBody)->next = jumpNode;

    return caseNode;
}

struct InstructionNode* parser::parse_default_case() {
    struct InstructionNode* defaultNode = new InstructionNode();
    expect(DEFAULT);
    expect(COLON);
    defaultNode->next = parse_body();
    defaultNode->type = NOOP;
    return defaultNode;
}

void parser::parse_inputs() {
    parse_num_list();
}

void parser::parse_num_list() {
    Token t = expect(NUM);
    inputs.push_back(std::stoi(t.lexeme)); //store inputs in inputs vector USING LOCATION TO CONVERT STRING NUM TO INTEGER Ex. "4" = 4
    Token t1 = lexer.peek(1);
    if (t1.token_type == NUM) {
        parse_num_list();
    }
}

struct InstructionNode * parse_Generate_Intermediate_Representation() {
    parser *p;
    p = new parser();
    struct InstructionNode* list;
    list = p->parse_program();
    return list; //TODO CHANGE THIS ONCE I START RETURNING INSTRUCTION NODES
}


