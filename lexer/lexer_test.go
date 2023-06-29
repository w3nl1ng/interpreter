package lexer

import (
	"monkey/token"
	"testing"
)

func TestNextToken1(t *testing.T) {
	input := "=+(){},;"

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.ASSIGN, "="},
		{token.PLUS, "+"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.LBRACE, "{"},
		{token.RBRACE, "}"},
		{token.COMMA, ","},
		{token.SEMICOLON, ";"},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong, expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - token literal wrong, expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestNextToken2(t *testing.T) {
	input := `let five = 5;
	let ten = 10;
	let add = fn(x, y) { 
		x + y 
	};
	let result = add(five, ten);
	`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.LET, "let"}, {token.IDENT, "five"}, {token.ASSIGN, "="}, {token.INT, "5"}, {token.SEMICOLON, ";"},
		{token.LET, "let"}, {token.IDENT, "ten"}, {token.ASSIGN, "="}, {token.INT, "10"}, {token.SEMICOLON, ";"},
		{token.LET, "let"}, {token.IDENT, "add"}, {token.ASSIGN, "="},
		{token.FUNCTION, "fn"}, {token.LPAREN, "("}, {token.IDENT, "x"},
		{token.COMMA, ","}, {token.IDENT, "y"}, {token.RPAREN, ")"},
		{token.LBRACE, "{"}, {token.IDENT, "x"}, {token.PLUS, "+"},
		{token.IDENT, "y"}, {token.RBRACE, "}"}, {token.SEMICOLON, ";"},
		{token.LET, "let"}, {token.IDENT, "result"}, {token.ASSIGN, "="},
		{token.IDENT, "add"}, {token.LPAREN, "("}, {token.IDENT, "five"},
		{token.COMMA, ","}, {token.IDENT, "ten"}, {token.RPAREN, ")"},
		{token.SEMICOLON, ";"},
	}

	l := New(input)
	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - token type wrong, expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - token literal wrong, expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}

}

func TestNextToken3(t *testing.T) {
	input := `
		let max = fn(x, y) {
			x == y;
			x != y;
			x > y;
			x < y;
			return false;
			return true;
			x = !x;
			x = x - 1;
			x = x * 1;
			x = x / 1;
			if(){}else(){}
		}
	`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.LET, "let"}, {token.IDENT, "max"}, {token.ASSIGN, "="},
		{token.FUNCTION, "fn"}, {token.LPAREN, "("}, {token.IDENT, "x"},
		{token.COMMA, ","}, {token.IDENT, "y"}, {token.RPAREN, ")"},
		{token.LBRACE, "{"}, {token.IDENT, "x"}, {token.EQ, "=="}, {token.IDENT, "y"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.NOT_EQ, "!="}, {token.IDENT, "y"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.GT, ">"}, {token.IDENT, "y"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.LT, "<"}, {token.IDENT, "y"}, {token.SEMICOLON, ";"},
		{token.RETURN, "return"}, {token.FALSE, "false"}, {token.SEMICOLON, ";"},
		{token.RETURN, "return"}, {token.TRUE, "true"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.ASSIGN, "="}, {token.BANG, "!"}, {token.IDENT, "x"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.ASSIGN, "="}, {token.IDENT, "x"}, {token.MINUS, "-"}, {token.INT, "1"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.ASSIGN, "="}, {token.IDENT, "x"}, {token.ASTERISK, "*"}, {token.INT, "1"}, {token.SEMICOLON, ";"},
		{token.IDENT, "x"}, {token.ASSIGN, "="}, {token.IDENT, "x"}, {token.SLASH, "/"}, {token.INT, "1"}, {token.SEMICOLON, ";"},
		{token.IF, "if"}, {token.LPAREN, "("}, {token.RPAREN, ")"}, {token.LBRACE, "{"}, {token.RBRACE, "}"},
		{token.ELSE, "else"}, {token.LPAREN, "("}, {token.RPAREN, ")"}, {token.LBRACE, "{"}, {token.RBRACE, "}"},
		{token.RBRACE, "}"},
	}

	l := New(input)
	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - token type wrong, expectedType=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - token literal wrong, expectedType=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}
