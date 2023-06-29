package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"
	IDENT   = "IDENT"
	INT     = "INT"

	ASSIGN = "="
	PLUS   = "+"

	COMMA     = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"

	LBRACE = "{"
	RBRACE = "}"

	FUNCTION = "FUNCTION"
	LET      = "LET"
	IF = "IF"
	ELSE = "ELSE"
	TRUE = "TRUE"
	FALSE = "FALSE"
	RETURN = "RETURN"

	EQ = "=="
	NOT_EQ = "!="

	MINUS = "-"
	BANG = "!"
	ASTERISK = "*"
	SLASH = "/"
	LT = "<"
	GT = ">"
)

var keywords = map[string]TokenType{
	"fn":  FUNCTION,
	"let": LET,
	"if": IF,
	"else": ELSE,
	"true": TRUE,
	"false": FALSE,
	"return": RETURN,
}

func LookupIdent(ident string) TokenType {
	if val, ok := keywords[ident]; ok {
		return val
	}
	return IDENT
}
