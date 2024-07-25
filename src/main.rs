use std::io;

#[derive(Debug, PartialEq)]
enum TokenType {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    Modulo,
    LParen,
    RParen,
    Variable(String),
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    value: String,
}

#[derive(Debug)]
enum ASTNode {
    Number(f64),
    BinaryOp(Box<ASTNode>, TokenType, Box<ASTNode>),
}

impl Token {
    fn new(token_type: TokenType, value: String) -> Self {
        Token { token_type, value }
    }
}

struct Tokenizer {
    input: String,
    position: usize,
}

impl Tokenizer {
    fn new(input: String) -> Self {
        Tokenizer { input, position: 0 }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let chars: Vec<char> = self.input.chars().collect();

        while self.position < chars.len() {
            let current_char = chars[self.position];

            if current_char.is_whitespace() {
                self.position += 1;
            } else if current_char.is_digit(10) || current_char == '.' {
                tokens.push(self.tokenize_number(&chars));
            } else if current_char.is_alphabetic() {
                tokens.push(self.tokenize_variable(&chars));
            } else {
                match current_char {
                    '+' => tokens.push(self.create_token(TokenType::Plus, current_char)),
                    '-' => tokens.push(self.create_token(TokenType::Minus, current_char)),
                    '*' => tokens.push(self.create_token(TokenType::Multiply, current_char)),
                    '/' => tokens.push(self.create_token(TokenType::Divide, current_char)),
                    '^' => tokens.push(self.create_token(TokenType::Power, current_char)),
                    '%' => tokens.push(self.create_token(TokenType::Modulo, current_char)),
                    '(' => tokens.push(self.create_token(TokenType::LParen, current_char)),
                    ')' => tokens.push(self.create_token(TokenType::RParen, current_char)),
                    '[' => tokens.push(self.create_token(TokenType::LParen, current_char)),
                    ']' => tokens.push(self.create_token(TokenType::RParen, current_char)),
                    _ => panic!("Unexpected character: {}", current_char),
                }
                self.position += 1;
            }
        }
        tokens
    }

    fn create_token(&self, token_type: TokenType, character: char) -> Token {
        Token::new(token_type, character.to_string())
    }

    fn tokenize_number(&mut self, chars: &[char]) -> Token {
        let start = self.position;
        while self.position < chars.len() && (chars[self.position].is_digit(10) || chars[self.position] == '.') {
            self.position += 1;
        }
        let number: String = chars[start..self.position].iter().collect();
        let value = number.parse::<f64>().unwrap();
        Token::new(TokenType::Number(value), number)
    }

    fn tokenize_variable(&mut self, chars: &[char]) -> Token {
        let start = self.position;
        while self.position < chars.len() && chars[self.position].is_alphanumeric() {
            self.position += 1;
        }
        let variable: String = chars[start..self.position].iter().collect();
        Token::new(TokenType::Variable(variable.clone()), variable)
    }
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 }
    }

    fn parse(&mut self) -> ASTNode {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> ASTNode {
        let mut node = self.parse_term();

        while self.position < self.tokens.len() {
            match self.tokens[self.position].token_type {
                TokenType::Plus => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Plus, Box::new(self.parse_term()));
                }
                TokenType::Minus => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Minus, Box::new(self.parse_term()));
                }
                _ => break,
            }
        }

        node
    }

    fn parse_term(&mut self) -> ASTNode {
        let mut node = self.parse_factor();

        while self.position < self.tokens.len() {
            match self.tokens[self.position].token_type {
                TokenType::Multiply => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Multiply, Box::new(self.parse_factor()));
                }
                TokenType::Divide => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Divide, Box::new(self.parse_factor()));
                }
                TokenType::Power => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Power, Box::new(self.parse_factor()));
                }
                TokenType::Modulo => {
                    self.position += 1;
                    node = ASTNode::BinaryOp(Box::new(node), TokenType::Modulo, Box::new(self.parse_factor()));
                }
                _ => break,
            }
        }

        node
    }

    fn parse_factor(&mut self) -> ASTNode {
        let token = &self.tokens[self.position];
        self.position += 1;

        match token.token_type {
            TokenType::Number(value) => ASTNode::Number(value),
            TokenType::LParen => {
                let node = self.parse_expression();
                self.position += 1;
                node
            }
            _ => panic!("Unexpected token: {:?}", token),
        }
    }
}

fn evaluate(node: &ASTNode) -> f64 {
    match node {
        ASTNode::Number(value) => *value,
        ASTNode::BinaryOp(left, op, right) => {
            let left_val = evaluate(left);
            let right_val = evaluate(right);

            match op {
                TokenType::Plus => left_val + right_val,
                TokenType::Minus => left_val - right_val,
                TokenType::Multiply => left_val * right_val,
                TokenType::Divide => left_val / right_val,
                TokenType::Power => left_val.powf(right_val),
                _ => panic!("Unexpected operator: {:?}", op),
            }
        }
    }
}

fn main() {
    let mut input : String = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read line");

    let mut tokenizer = Tokenizer::new(input.to_string());
    let tokens = tokenizer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    let result = evaluate(&ast);
    println!("Result: {}", result);
}

