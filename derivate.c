#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>

#define INPUT_SIZE 256

typedef enum { ADD, MUL, DIV, POW, SUB, VAR, LPAR, RPAR, ENDT, CONST, SIN, COS, EXP, LOG, SQRT, MINUS, STARTT } TokenType;

typedef struct
{
    TokenType type;
    float value;
}
token;

typedef struct node
{
    struct node* left;
    struct node* right;
    token data;
}
node;

void print_ast(node* root, int space) {
    if (root == NULL) {
        return;
    }

    space += 5;

    if (root->right != NULL) {
        print_ast(root->right, space);
    }

    printf("\n");
    for (int i = 5; i < space; i++) {
        printf(" ");
    }

    switch (root->data.type) {
        case CONST: printf("[%.2f]", root->data.value); break;
        case VAR:   printf("[x]"); break;
        case ADD:   printf("[ + ]"); break;
        case SUB:   printf("[ - ]"); break;
        case MUL:   printf("[ * ]"); break;
        case DIV:   printf("[ / ]"); break;
        case POW:   printf("[ ^ ]"); break;
        case MINUS: printf("[-unary]"); break;
        case SIN:   printf("[sin]"); break;
        case COS:   printf("[cos]"); break;
        case EXP:   printf("[exp]"); break;
        case LOG:   printf("[log]"); break;
        case SQRT:  printf("[sqrt]"); break;
        default:    printf("[?]"); break;
    }

    print_ast(root->left, space);
}

TokenType give_token(char* c, int* i, TokenType T)
{
    if(strncmp(&c[*i], "sin", 3) == 0){(*i) += 3; return SIN;}
    if(strncmp(&c[*i], "cos", 3) == 0){(*i) += 3; return COS;}
    if(strncmp(&c[*i], "exp", 3) == 0){(*i) += 3; return EXP;}
    if(strncmp(&c[*i], "log", 3) == 0){(*i) += 3; return LOG;}
    if(strncmp(&c[*i], "sqrt", 4) == 0){(*i) += 4; return SQRT;}

    if(c[*i] == '-' && (T == STARTT || T == LPAR || T == MUL || T == DIV || T == ADD || T == SUB || T == POW || T == MINUS)){
        
        (*i)++;
        return MINUS;
    }

    switch(c[*i]){

        case '+': (*i)++; return ADD;
        case '*': (*i)++; return MUL;
        case '-': (*i)++; return SUB;
        case '/': (*i)++; return DIV;
        case '^': (*i)++; return POW;
        case 'x': (*i)++; return VAR;
        case '(': (*i)++; return LPAR;
        case ')': (*i)++; return RPAR;
        case '\0': (*i)++; return ENDT;
        default : fprintf(stderr, "Syntax error (Unknown character)."); exit(1);
    }

}

float parse_value(char* expression, int* i)
{

float value = 0;
float factor = 0.1;

while(isdigit(expression[*i])){

    value = (value * 10) + (expression[*i] - '0');
    (*i)++;
}

if(expression[*i] == '.'){

    (*i)++;

    while(isdigit(expression[*i])){

        value += factor * (expression[*i] - '0');
        factor /= 10;
        (*i)++;
    }
}

return value;
}

token* lexer(char* expression)
{

token* tokens = NULL;
int size = 1;
int i = 0;
int j = 0;

while(expression[i] != '\0'){

    if(!isdigit(expression[i])){

        if(expression[i] == ' '){ 

            i++;
        }
        else{

            tokens = realloc(tokens, sizeof(token) * size);

            if(tokens == NULL){

                fprintf(stderr, "Memory allocation failed !\n");
                exit(1);   
            }

            if(j == 0){ 
                tokens[j].type = give_token(expression, &i, STARTT);}
            else{
                tokens[j].type = give_token(expression, &i, tokens[j-1].type);
            }

            j++; size++;
        }
    }
    else{

        tokens = realloc(tokens, sizeof(token) * size);

        if(tokens == NULL){

            fprintf(stderr, "Memory allocation failed !\n");
            exit(1);   
        }

        tokens[j].type = CONST;
        tokens[j].value = parse_value(expression, &i);
        j++; size++;
    }
}

tokens = realloc(tokens, sizeof(tokens) * size);

if(j == 0){
    tokens[j].type = give_token(expression, &i, STARTT);
}
else{
    tokens[j].type = give_token(expression, &i, tokens[j-1].type);
}

return tokens;
}

void print_tokens(token* tokens)
{

printf("\n");
while(tokens->type != ENDT){

    switch(tokens->type){

        case ADD : printf(" +"); break;
        case DIV : printf(" /"); break;
        case SUB : case MINUS : printf(" -"); break;
        case MUL : printf(" *"); break;
        case POW : printf(" ^"); break;
        case VAR : printf(" x"); break;
        case CONST : printf(" %.2f", tokens->value); break;
        case LPAR : printf(" ("); break;
        case RPAR : printf(" )"); break;
        case SQRT : printf(" sqrt"); break;
        case COS : printf(" cos"); break;
        case SIN : printf(" sin"); break;
        case EXP : printf(" exp"); break;
        case LOG : printf(" log"); break;
        default : printf(" ?"); break;
    }
    tokens++;
}

}

node* create_node(token data, node* left, node* right)
{
    node* created_node = malloc(sizeof(node));

    if(created_node == NULL){

        fprintf(stderr, "Memory allocation failed !\n");
        exit(1);
    }

    created_node->left = left;
    created_node->right = right;
    created_node->data = data;

    return created_node;
}

node* parse_expression(token** tokens);

node* parse_factor(token** tokens)
{   
    node* power = NULL;
    node* base = NULL;
    node* factor = NULL;
    node* argument = NULL;
    node* negatif_node = NULL;

    if((*tokens)->type == LPAR){

        (*tokens)++;
        base = parse_expression(tokens);
        (*tokens)++;
    }
    else{

        if((*tokens)->type == MINUS){

            base = create_node((**tokens), NULL, NULL);
            (*tokens)++;

            if((*tokens)->type == LPAR){

                (*tokens)++;
                negatif_node = parse_expression(tokens);
                base->left = negatif_node;
                (*tokens)++;
            }
            else{

                negatif_node = parse_factor(tokens);
                base->left = negatif_node;
            }
        }
        else if((*tokens)->type != CONST && (*tokens)->type != VAR){

            base = create_node((**tokens), NULL, NULL);
            (*tokens) += 2;
            argument = parse_expression(tokens);
            base->left = argument;
            (*tokens)++;
        }
        else{

            base = create_node((**tokens), NULL, NULL);
            (*tokens)++;
        }
    }

    if((*tokens)->type == POW){

        factor = create_node((**tokens), base, NULL);
        (*tokens)++;

        power = create_node((**tokens), NULL, NULL);
        (*tokens)++;

        factor->right = power;
        base = factor;
    }

    return base;
}

int get_priority_level(TokenType operation)
{
    switch(operation){

        case ADD : case SUB : return 1;
        case MUL : case DIV : return 2;
        case POW : return 3;
        case MINUS : return 2;
        case COS : case SIN : case EXP : case LOG : case SQRT : return 5;
        case VAR : case CONST : return 6;
    }

    return 0;
}

node* parse_term(token** tokens)
{   
    node* left_factor = NULL;
    node* right_factor = NULL;
    node* term = NULL;

    left_factor = parse_factor(tokens);

    while((*tokens)->type == MUL || (*tokens)->type == DIV){

        term = create_node((**tokens), left_factor, NULL);
        (*tokens)++;

        right_factor = parse_factor(tokens);

        term->right = right_factor;
        left_factor = term;
    }

    return left_factor;
}

node* parse_expression(token** tokens)
{   
    node* left_term = NULL;
    node* right_term = NULL;
    node* expression = NULL;

    left_term = parse_term(tokens);

    while((*tokens)->type == ADD || (*tokens)->type == SUB){

        expression = create_node((**tokens), left_term, NULL);
        (*tokens)++;

        right_term = parse_term(tokens);

        expression->right = right_term;
        left_term = expression;
    }

    return left_term;
}

void free_node(node** to_free)
{   
    if (to_free == NULL || *to_free == NULL) {

        return;
    }

    if((*to_free)->left != NULL){
    
        free_node(&((*to_free)->left));
    }

    if((*to_free)->right != NULL){
    
        free_node(&((*to_free)->right));
    }
    
    free(*to_free);
    (*to_free) = NULL;
}

node* duplicate_node(node* root)
{
    node* duplicated_node = NULL;

    if(root == NULL){

        return duplicated_node;
    }

    node* left_node = duplicate_node(root->left);
    node* right_node = duplicate_node(root->right);

    duplicated_node = create_node(root->data, left_node, right_node);

    return duplicated_node;
}

node* derivate(node* root);

node* derivate_const(node* root)
{   
    token data;
    data.type = CONST;
    data.value = 0;

    node* derivated_node = create_node(data, NULL, NULL);
    
    return derivated_node;
}

node* derivate_var(node* root)
{   
    token data;
    data.type = CONST;
    data.value = 1;

    node* derivated_node = create_node(data, NULL, NULL);

    return derivated_node;
}

node* derivate_sum(node* operation)
{    
    node* left_derivate = derivate(operation->left);
    node* right_derivate = derivate(operation->right);

    node* derivated_node = create_node(operation->data, left_derivate, right_derivate);

    return derivated_node;
}

node* derivate_product(node* operation)
{   
    node* left_derivate = derivate(operation->left);
    node* right_derivate = derivate(operation->right);

    node* left = duplicate_node(operation->left);
    node* right = duplicate_node(operation->right);

    node* right_product = create_node(operation->data, left, right_derivate);
    node* left_product = create_node(operation->data, left_derivate, right);

    token sum;
    sum.type = ADD;

    node* derivated_node = create_node(sum, left_product, right_product);

    return derivated_node;
}

node* derivate_minus(node* operation)
{
    node* negatif_node = derivate(operation->left);

    node* derivated_node = create_node(operation->data, negatif_node, NULL);

    return derivated_node;
}

node* derivate_fraction(node* operation)
{   
    token product;
    product.type = MUL;

    token difference;
    difference.type = SUB;

    token power;
    power.type = POW;

    token two;
    two.type = CONST;
    two.value = 2;

    node* left_derivate = derivate(operation->left);
    node* right_derivate = derivate(operation->right);

    node* left = duplicate_node(operation->left);
    node* right = duplicate_node(operation->right);

    node* right_product = create_node(product, left, right_derivate);
    node* left_product = create_node(product, left_derivate, right);

    node* numerator = create_node(difference, left_product, right_product);

    node* base = duplicate_node(operation->right); 
    node* exponent = create_node(two, NULL, NULL);

    node* denominator = create_node(power, base, exponent);

    node* derivated_node = create_node(operation->data, numerator, denominator);

    return derivated_node;
}

node* derivate_power(node* operation)
{   
    token difference;
    difference.type = SUB;

    token product;
    product.type = MUL;

    token one;
    one.type = CONST;
    one.value = 1;

    node* base = duplicate_node(operation->left);

    node* derivated_base = derivate(operation->left);

    node* factor = duplicate_node(operation->right);
    node* term = duplicate_node(operation->right);

    node* one_node = create_node(one, NULL, NULL);
    node* exponent = create_node(difference, term, one_node);

    node* power = create_node(operation->data, base, exponent);
    node* derivated_node = create_node(product, derivated_base, power);
    derivated_node = create_node(product, factor, derivated_node);

    return derivated_node;
}

node* derivate_sin(node* function)
{
    token product;
    product.type = MUL;

    token cos_function;
    cos_function.type = COS;

    node* argument = duplicate_node(function->left);
    node* derivated_argument = derivate(function->left);

    node* derivated_node = create_node(cos_function, argument, NULL);
    derivated_node = create_node(product, derivated_argument, derivated_node);

    return derivated_node;
}

node* derivate_exp(node* function)
{
    token product;
    product.type = MUL;

    node* argument = duplicate_node(function->left);
    node* derivated_argument = derivate(function->left);

    node* derivated_node = create_node(function->data, argument, NULL);
    derivated_node = create_node(product, derivated_argument, derivated_node);

    return derivated_node;
}

node* derivate_log(node* function)
{
    token fraction;
    fraction.type = DIV;

    node* argument = duplicate_node(function->left);
    node* derivated_argument = derivate(function->left);

    node* derivated_node = create_node(fraction, derivated_argument, argument);

    return derivated_node;
}

node* derivate_sqrt(node* function)
{
    token fraction;
    fraction.type = DIV;

    token product;
    product.type = MUL;

    token two;
    two.type = CONST;
    two.value = 2;

    node* argument = duplicate_node(function->left);
    node* derivated_argument = derivate(function->left);

    node* square_root = create_node(function->data, argument, NULL);
    node* two_node = create_node(two, NULL, NULL);

    node* denominator = create_node(product, two_node, square_root);
    node* derivated_node = create_node(fraction, derivated_argument, denominator);

    return derivated_node;
}

node* derivate_cos(node* function)
{
    token product;
    product.type = MUL;

    token minus;
    minus.type = MINUS;

    token sin_function;
    sin_function.type = SIN;

    node* argument = duplicate_node(function->left);
    node* derivated_argument = derivate(function->left);

    node* derivated_node = create_node(sin_function, argument, NULL);
    derivated_node = create_node(product, derivated_argument, derivated_node);
    derivated_node = create_node(minus, derivated_node, NULL);

    return derivated_node;
}

node* derivate(node* root)
{   
    node* derivated_node = NULL;

    switch(root->data.type)
    {
        case CONST : derivated_node = derivate_const(root); break;
        case VAR : derivated_node = derivate_var(root); break;
        case ADD : case SUB : derivated_node = derivate_sum(root); break;
        case MUL : derivated_node = derivate_product(root); break;
        case DIV : derivated_node = derivate_fraction(root); break;
        case MINUS : derivated_node = derivate_minus(root); break;
        case POW : derivated_node = derivate_power(root); break;
        case SIN : derivated_node = derivate_sin(root); break;
        case COS : derivated_node = derivate_cos(root); break;
        case EXP : derivated_node = derivate_exp(root); break;
        case LOG : derivated_node = derivate_log(root); break;
        case SQRT : derivated_node = derivate_sqrt(root); break;
    }

    return derivated_node;
}

void calculate_const(node* root)
{   
    float b = 0;

    if(root->right != NULL){

        b = ((root->right)->data).value;
    }

    float a = ((root->left)->data).value; 

    switch((root->data).type)
    {
        case MUL : (root->data).value = a * b; break;
        case ADD : (root->data).value = a + b; break;
        case SUB : (root->data).value = a - b; break;
        case DIV : (root->data).value = a / b; break;
        case POW : (root->data).value = powf(a,b); break;
        case COS : (root->data).value = cosf(a); break;
        case SIN : (root->data).value = sinf(a); break;
        case SQRT : (root->data).value = sqrtf(a); break;
        case EXP : (root->data).value = expf(a); break;
        case LOG : (root->data).value = logf(a); break;
    }

    (root->data).type = CONST;

    free_node(&(root->left));
    free_node(&(root->right));

    root->left = NULL;
    root->right = NULL;
}

void null_product(node* root)
{
    (root->data).type = CONST;
    (root->data).value = 0;

    free_node(&(root->left));
    free_node(&(root->right));

    root->left = NULL;
    root->right = NULL;
}

void null_power(node* root)
{
    (root->data).type = CONST;
    (root->data).value = 1;

    free_node(&(root->left));
    free_node(&(root->right));

    root->left = NULL;
    root->right = NULL;
}

void identity(node** root)
{
    node* factor;

    if((((*root)->left)->data).type == CONST){

        factor = duplicate_node((*root)->right);
    }
    else if((((*root)->right)->data).type == CONST){

        factor = duplicate_node((*root)->left);
    }

    free_node(root);

    (*root) = factor;
}

void negatif_identity(node** root)
{
    node* factor;

    if((((*root)->left)->data).type == CONST){

        factor = duplicate_node((*root)->right);
    }
    else if((((*root)->right)->data).type == CONST){

        factor = duplicate_node((*root)->left);
    }

    free_node(root);

    token minus_data;
    minus_data.type = MINUS;

    (*root) = create_node(minus_data, factor, NULL);
}

void log_sum(node* root)
{
    token data;
    
    if((root->data).type == ADD){
    
        data.type = MUL;
    }
    else{
    
        data.type = DIV;
    }

    (root->data).type = LOG;
    
    node* product = create_node(data, NULL, NULL);
    
    node* left = duplicate_node((root->left)->left);
    node* right = duplicate_node((root->right)->left);
    
    product->left = left;
    product->right = right;
    
    free_node(&(root->left));
    free_node(&(root->right));
    
    root->left = product;
}

void expo_product(node* root)
{
    token data;
    
    if((root->data).type == MUL){
    
        data.type = ADD;
    }
    else{
    
        data.type = SUB;
    }

    (root->data).type = EXP;
    
    node* sum = create_node(data, NULL, NULL);
    
    node* left = duplicate_node((root->left)->left);
    node* right = duplicate_node((root->right)->left);
    
    sum->left = left;
    sum->right = right;
    
    free_node(&(root->left));
    free_node(&(root->right));
    
    root->left = sum;
}

void sqrt_product(node* root)
{
    token data;
    
    if((root->data).type == MUL){
    
        data.type = MUL;
    }
    else{
    
        data.type = DIV;
    }

    (root->data).type = SQRT;
    
    node* product = create_node(data, NULL, NULL);
    
    node* left = duplicate_node((root->left)->left);
    node* right = duplicate_node((root->right)->left);
    
    product->left = left;
    product->right = right;
    
    free_node(&(root->left));
    free_node(&(root->right));
    
    root->left = product;
}

void neg_to_pos(node** root)
{
    node* positif_node = duplicate_node(((*root)->left)->left);
    free_node(root);
    (*root) = positif_node;    
}

void log_power(node** root)
{
    node* exponant = duplicate_node(((*root)->left)->right);
    node* base = duplicate_node(((*root)->left)->left);
    
    token data;
    data.type = LOG;
    
    node* log_node = create_node(data, base, NULL);
    
    data.type = MUL;
    
    node* product = create_node(data, exponant, log_node);
    
    free_node(root);
    (*root) = product;
}

void power_product(node** root)
{
    node* new_power = duplicate_node((*root)->left);
    
    if(((*root)->data).type == MUL){
    
        ((new_power->right)->data).value += ((((*root)->right)->right)->data).value;
    }
    else{
    
        ((new_power->right)->data).value -= ((((*root)->right)->right)->data).value;
    }
    
    free_node(root);
    
    (*root) = new_power;
}

void double_power(node* root)
{
    node* base = duplicate_node((root->left)->left);
    
    ((root->right)->data).value *= (((root->left)->right)->data).value;
    
    free_node(&(root->left));
    
    root->left = base;
}

void expo_power(node** root)
{
    node* left = duplicate_node(((*root)->left)->left);
    node* right = duplicate_node((*root)->right);
    
    token data;
    data.type = MUL;
    
    node* product_node = create_node(data, left, right);
    
    data.type = EXP;
    
    node* expo_node = create_node(data, product_node, NULL);
    
    free_node(root);
    (*root) = expo_node;
}

void inverse_function(node** root)
{
    node* argument = duplicate_node(((*root)->left)->left);
    
    free_node(root);
    (*root) = argument;
}

bool compare_nodes(node* node_a, node* node_b)
{
    if(node_a == NULL && node_b == NULL){

        return true;
    } 

    if(node_a == NULL || node_b == NULL){

        return false;
    } 

    if((node_a->data).type != (node_b->data).type){

        return false;
    }

    if((node_a->data).type == CONST && (node_a->data).value != (node_b->data).value){

        return false;
    }

    if(compare_nodes(node_a->left, node_b->left) == false || compare_nodes(node_a->right, node_b->right) == false){

        return false;
    }

    return true;
}

void flattering(node* root, node*** flattered_node, TokenType operation, int* size)
{
    if((root->data).type == operation){

        flattering(root->right, flattered_node, operation, size);
        flattering(root->left, flattered_node, operation, size);           
    }
    else if(root != NULL){

        (*flattered_node) = realloc((*flattered_node), sizeof(node*) * (*size));
        (*flattered_node)[(*size) - 1] = duplicate_node(root);
        (*size)++;
    }
}

void put_to_power(node** base, int exponant)
{   
    if(exponant == 1){

        return;
    }

    token power; 
    power.type = POW;
 
    token exponant_data; 
    exponant_data.type = CONST; 
    exponant_data.value = (float)exponant;    
    
    node* base_node = duplicate_node(*base);
    node* exponant_node = create_node(exponant_data, NULL, NULL);
    
    node* power_node = create_node(power, base_node, exponant_node);
    
    free_node(base);
    (*base) = power_node;
}

void put_to_factor(node** base, int factor)
{   
    if(factor == 1){

        return;
    }

    token product; 
    product.type = MUL;
 
    token factor_data; 
    factor_data.type = CONST; 
    factor_data.value = (float)factor;    
    
    node* base_node = duplicate_node(*base);
    node* factor_node = create_node(factor_data, NULL, NULL);
    
    node* product_node = create_node(product, base_node, factor_node);
    
    free_node(base);
    (*base) = product_node;
}

bool simplify(node** root, bool neg);

void simplify_substraction(node* root)
{  
    if(root == NULL){

        return;
    }

    simplify_substraction(root->left);
    simplify_substraction(root->right);

    token product_data;
    product_data.type = MUL;

    token minus_one;
    minus_one.type = CONST;
    minus_one.value = -1;

    if((root->data).type == SUB){

        (root->data).type = ADD;

        if(((root->right)->data).type == CONST){

            ((root->right)->data).value *= -1;
        }
        else{

            node* negatif_node = duplicate_node(root->right);
            free_node(&(root->right));
            node* const_node = create_node(minus_one, NULL, NULL);
            root->right = create_node(product_data, negatif_node, const_node);
        }
    }
    else if((root->data).type == MINUS){

        (root->data).type = MUL;
        root->right = create_node(minus_one, NULL, NULL);
    }
}

void simplify_flattered_node(node*** flattered_node, TokenType operation, int* size, float* constants)
{   
    int i, j;

    node** simplified_table = NULL;
    int new_size = 1;

    float occurences;

    if(operation == MUL){

        (*constants) = 1;
    }
    else{

        (*constants) = 0;
    }

    for(i = 0; i < (*size - 1); i++){

        node* current = duplicate_node((*flattered_node)[i]);

        simplify(&current, false);

        free_node(&(*flattered_node)[i]);

        if(current != NULL){

            if((current->data).type == CONST){

                if(operation == MUL){

                    (*constants) *= (current->data).value;
                }
                else{

                    (*constants) += (current->data).value;
                }
            }
            else{

                if((current->data).type == POW && operation == MUL){

                    occurences = ((current->right)->data).value;
                    node* tmp = current;
                    current = duplicate_node(current->left);
                    free_node(&tmp);
                }
                else if((current->data).type == MUL && operation == ADD && ((current->right)->data).type == CONST){

                    occurences = ((current->right)->data).value;
                    node* tmp = current;
                    current = duplicate_node(current->left);
                    free_node(&tmp);
                }  
                else{

                    occurences = 1;
                }

                for(j = 0; j < (*size - 1); j++){
                    
                    if((*flattered_node)[j] != NULL && ((*flattered_node)[j]->data).type == POW && operation == MUL){
                        
                        if(compare_nodes(current, (*flattered_node)[j]->left)){
                            
                            occurences += (((*flattered_node)[j]->right)->data).value;
                            free_node(&(*flattered_node)[j]);
                        }
                    }
                    else if((*flattered_node)[j] != NULL && ((*flattered_node)[j]->data).type == MUL && operation == ADD && (((*flattered_node)[j]->right)->data).type == CONST){

                        bool recall = simplify(&((*flattered_node)[j]->left), false);

                        while(recall){

                            recall = simplify(&((*flattered_node)[j]->left), false);
                        }

                        if(compare_nodes(current, (*flattered_node)[j]->left)){

                            occurences += (((*flattered_node)[j]->right)->data).value;
                            free_node(&(*flattered_node)[j]);
                        }                        
                    }
                    else{

                        if(compare_nodes(current, (*flattered_node)[j])){
                            
                            free_node(&(*flattered_node)[j]);
                            occurences++;
                        }
                    }
                }

                simplified_table = realloc(simplified_table, sizeof(node*) * new_size);
            
                if(operation == MUL){

                    put_to_power(&current, occurences);
                    simplified_table[new_size - 1] = current;
                }
                else{

                    put_to_factor(&current, occurences);
                    simplified_table[new_size - 1] = current;
                }

                new_size++;
            }
        }
    }

    free(*flattered_node);
    (*flattered_node) = simplified_table;
    (*size) = new_size;
}

node* rebuild_node(node** flattered_table, int size, float constants,TokenType operation)
{
    int i;

    token data_const;
    data_const.type = CONST;
    data_const.value = constants;

    token data;
    data.type = operation;

    node* operation_node;

    node* constant_node = create_node(data_const, NULL, NULL);
    node* root = create_node(data, NULL, constant_node);
    node* previous = root;

    for(i = 0; i < (size - 1); i++){

        if(flattered_table[i] != NULL){

            operation_node = create_node(data, NULL, flattered_table[i]);
            previous->left = operation_node;
            previous = operation_node;
        }
    }

    token data_neutral;
    data_neutral.type = CONST;

    if(operation == ADD){

        data_neutral.value = 0;
    }
    else{

        data_neutral.value = 1;
    }

    node* neutral = create_node(data_neutral, NULL, NULL);

    previous->left = neutral;

    bool recall = simplify(&root, false);

    while(recall){

        recall = simplify(&root, false);
    }

    return root;
}

bool simplify(node** root, bool neg);

void simplify_product_and_sum(node** root)
{   
    if((*root) == NULL){

        return;
    }

    simplify_product_and_sum(&((*root)->left));
    simplify_product_and_sum(&((*root)->right));

    TokenType operation = ((*root)->data).type; 
    node** flattered_table = NULL;
    int size = 1;
    float constants;

    if(operation == ADD || operation == MUL){

        flattering((*root), &flattered_table, operation, &size);

        simplify_flattered_node(&flattered_table, operation, &size, &constants);

        free_node(root);

        (*root) = rebuild_node(flattered_table, size, constants, operation);

        free(flattered_table);
    }
}

void add_to_sub(node* root)
{
    node* negatif_node = duplicate_node((root->left)->left);
    free_node(&(root->left));
    (root->data).type = SUB;
    root->left = negatif_node;
}

bool simplify(node** root, bool neg)
{  
    if((*root) == NULL || ((*root)->left == NULL && (*root)->right == NULL)){

        return false;
    }

    bool recall = false;

    TokenType left_type, right_type, left_base_type, right_base_type;
    float left_value, right_value, left_exponant_value, right_exponant_value;
    
    if((*root)->left != NULL){
    
        left_type = (((*root)->left)->data).type;
        left_value = (((*root)->left)->data).value;
    }
    
    if((*root)->right != NULL){
        
        right_type = (((*root)->right)->data).type;
        right_value = (((*root)->right)->data).value;
    }    
    
    TokenType operation = ((*root)->data).type;

    if((left_type == CONST && right_type == CONST) || (left_type == CONST && (*root)->right == NULL)){

        calculate_const(*root);
        recall = true;
    }
    else if(operation == MINUS && left_type == MINUS){

        neg_to_pos(root);
        recall = true;
    }
    else if(neg == true && operation == ADD && left_type == MINUS){

        add_to_sub(*root);
        recall = true;
    }
    else if(((left_type == CONST && left_value == -1  && operation == MUL) || (right_type == CONST && right_value == -1 && (operation == MUL || operation == DIV))) && neg == true){
        
        negatif_identity(root);
        recall = true;
    }
    else if((left_type == CONST && left_value == 0  && (operation == MUL || operation == DIV)) || (right_type == CONST && right_value == 0 && operation == MUL)){

        null_product(*root);
        recall = true;
    }
    else if(((operation == LOG || operation == COS) && left_type == CONST && left_value == 1) || ((operation == SIN || operation == SQRT) && left_type == CONST && left_value == 0)){
    
        null_product(*root);
        recall = true;
    }
    else if((right_type == CONST && right_value == 0 && operation == POW) || (left_type == CONST && left_value == 0 && operation == EXP) || (left_type == CONST && left_value == 1 && operation == SQRT)){
    
        null_power(*root);
        recall = true;
    }
    else if((left_type == CONST && left_value == 0  && operation == ADD) || (right_type == CONST && right_value == 0 && (operation == ADD || operation == SUB))){

        identity(root);
        recall = true;
    }
    else if((left_type == CONST && left_value == 1  && operation == MUL) || (right_type == CONST && right_value == 1 && (operation == MUL || operation == DIV || operation == POW))){

        identity(root);
        recall = true;
    }
    else if(left_type == EXP && right_type == EXP && (operation == MUL || operation == DIV)){

        expo_product(*root);
        recall = true;
    }
    else if(left_type == SQRT && right_type == SQRT && (operation == MUL || operation == DIV)){

        sqrt_product(*root);
        recall = true;
    }
    else if(left_type == EXP && operation == POW){

        expo_power(root);
        recall = true;
    }
    else if(left_type == LOG && right_type == LOG && (operation == ADD || operation == SUB)){

        log_sum(*root);
        recall = true;
    }
    else if(left_type == POW && right_type == POW && (operation == MUL || operation == DIV) && compare_nodes(((*root)->right)->left, ((*root)->left)->left)){

        power_product(root);
        recall = true;
    }
    else if(left_type == POW && operation == POW){

        double_power(*root);
        recall = true;
    }
    else if(left_type == POW && operation == LOG){

        log_power(root);
        recall = true;
    }
    else if((left_type == EXP && operation == LOG) || (left_type == LOG && operation == EXP)){

        inverse_function(root);
        recall = true;
    }
    else if(left_type == POW && right_type == POW && operation == ADD){
    
        if(((*root)->left)->left != NULL){
        
            left_base_type = ((((*root)->left)->left)->data).type;
        }
        if(((*root)->right)->left != NULL){
        
            right_base_type = ((((*root)->right)->left)->data).type;
        }
        if(((*root)->left)->right != NULL){
        
            left_exponant_value = ((((*root)->left)->right)->data).value;
        }
        if(((*root)->right)->right != NULL){
        
            right_exponant_value = ((((*root)->right)->right)->data).value;
        }
        
        if(((left_base_type == COS && right_base_type == SIN) || (left_base_type == SIN && right_base_type == COS)) && left_exponant_value == 2 && right_exponant_value == 2){
        
            if(compare_nodes((((*root)->left)->left)->left, (((*root)->right)->left)->left)){
            
                null_power(*root);
                recall = true;                
            }
        }
    }
    
    bool is_left_true = simplify(&((*root)->left), neg);
    bool is_right_true = simplify(&((*root)->right), neg);

    if(is_left_true || is_right_true){
        
        recall = true;
    }

    return recall;
}

node* product_of_two_nodes(node* fnode, node* snode){

    if(fnode == NULL || snode == NULL){

        return NULL;
    }

    node* left_node = duplicate_node(fnode);
    node* right_node = duplicate_node(snode);

    token product_data;
    product_data.type = MUL;

    node* product_node = create_node(product_data, left_node, right_node);

    return product_node;
}

void distribute(node** root)
{
    if((*root) == NULL){

        return;
    }

    distribute(&((*root)->left));
    distribute(&((*root)->right));

    if(((*root)->data).type != MUL || ((((*root)->left)->data).type != ADD && (((*root)->right)->data).type != ADD)){

        return;
    } 

    node** right_flattered_table = NULL;
    int right_size = 1;

    node** left_flattered_table = NULL;
    int left_size = 1;

    flattering((*root)->left, &left_flattered_table, ADD, &left_size);
    flattering((*root)->right, &right_flattered_table, ADD, &right_size);

    int new_flattered_table_size = (left_size - 1) * (right_size - 1);
    node** new_flattered_table = malloc(sizeof(node*) * new_flattered_table_size);

    int i, j, k;
    k = 0;
    
    for(i = 0; i < (left_size - 1); i++){
        
        for(j = 0; j < (right_size - 1); j++){

            new_flattered_table[k] = product_of_two_nodes(left_flattered_table[i], right_flattered_table[j]);
            k++;
        }
    }

    free_node(root);

    (*root) = rebuild_node(new_flattered_table, new_flattered_table_size + 1, 0, ADD);

    free(new_flattered_table);
    free(left_flattered_table);
    free(right_flattered_table);
}

void add_token(token** tokens, int* size, TokenType token_type, float value)
{
    (*tokens) = realloc((*tokens), sizeof(token) * (*size));
    (*tokens)[*size - 1].type = token_type;
    (*tokens)[*size - 1].value = value;
    (*size)++;    
}

void pretty_printer(node* root, token** tokens, int* size, int parent_priority)
{   
    if(root == NULL){

        return;
    }

    int root_priority = get_priority_level((root->data).type);

    if(parent_priority > root_priority && parent_priority != 5){

        add_token(tokens, size, LPAR, 0);
    }

    if((root->data).type == CONST || (root->data).type == VAR){

        add_token(tokens, size, (root->data).type, (root->data).value);
    }
    else if((root->data).type == ADD || (root->data).type == MUL || (root->data).type == POW){

        pretty_printer(root->left, tokens, size, root_priority);
        add_token(tokens, size, (root->data).type, 0);
        pretty_printer(root->right, tokens, size, root_priority);
    }
    else if((root->data).type == DIV || (root->data).type == SUB){

        pretty_printer(root->left, tokens, size, root_priority);
        add_token(tokens, size, (root->data).type, 0);
        pretty_printer(root->right, tokens, size, root_priority + 1);    
    }
    else if((root->data).type == COS || (root->data).type == COS || (root->data).type == EXP || (root->data).type == LOG || (root->data).type == SIN || (root->data).type == SQRT){

        add_token(tokens, size, (root->data).type, 0);
        add_token(tokens, size, LPAR, 0);
        pretty_printer(root->left, tokens, size, root_priority);
        add_token(tokens, size, RPAR, 0);
    }
    else if((root->data).type == MINUS){

        add_token(tokens, size, (root->data).type, 0);
        pretty_printer(root->left, tokens, size, root_priority);

    }

    if(parent_priority > root_priority && parent_priority != 5){

        add_token(tokens, size, RPAR, 0);
    }
}

int main()
{
char expression[INPUT_SIZE];

printf("\nEnter the function you want to derivate : ");
fgets(expression, sizeof(expression), stdin);
expression[strcspn(expression, "\n")] = '\0';

token* tokens = lexer(expression);

token* index = tokens;

node* AST = parse_expression(&index);

simplify_substraction(AST);

distribute(&AST);

simplify_product_and_sum(&AST);

bool recall = simplify(&AST, true);

while(recall){

    recall = simplify(&AST, true);
}

node* derivated_AST = derivate(AST);

simplify_substraction(derivated_AST);
simplify_substraction(derivated_AST);

distribute(&derivated_AST);

simplify_product_and_sum(&derivated_AST);

recall = simplify(&derivated_AST, true);

while(recall){

    recall = simplify(&derivated_AST, true);
}

tokens = NULL;
int size = 1;

pretty_printer(derivated_AST, &tokens, &size, 0);

add_token(&tokens, &size, ENDT, 0);

print_tokens(tokens);

free_node(&AST);

return 0;
}