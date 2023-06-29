package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"testing"
)

func TestLetStatements1(t *testing.T) {
	tests := []struct {
		input string
		name  string
		value interface{}
	}{
		{"let x = 5;", "x", 5},
		{"let forbar = false;", "forbar", false},
		{"let y = x;", "y", "x"},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if len(program.Statements) != 1 {
			t.Fatalf("len(program.Statements) is not 1, got=%d\n",
				len(program.Statements))
		}

		testLetStatement(t, program.Statements[0], tt.name, tt.value)
	}
}

func testLetStatement(t *testing.T, stmt ast.Statement, name string, value interface{}) bool {
	if stmt.TokenLiteral() != "let" {
		t.Errorf("stmt.TokenLiteral() not 'let', got=%q\n", stmt.TokenLiteral())
		return false
	}

	letState, ok := stmt.(*ast.LetStatement)
	if !ok {
		t.Errorf("stmt is not *ast.LetStatement, got=%T\n", stmt)
		return false
	}

	if letState.Name.Value != name {
		t.Errorf("letState.Name.Value() not %s, got=%s\n", name, letState.Name.Value)
		return false
	}
	if letState.Name.TokenLiteral() != name {
		t.Errorf("letState.Name.TokenLiteral() not %s, got=%s\n", name, letState.Name.TokenLiteral())
		return false
	}

	if !testLiteralExpression(t, letState.Value, value) {
		return false
	}

	return true
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	} else {
		t.Errorf("parser has %d errors\n", len(errors))
		for _, msg := range errors {
			t.Errorf("parser error: %q\n", msg)
		}
		t.FailNow()
	}
}

func TestReturnStatements1(t *testing.T) {
	tests := []struct {
		input       string
		returnValue interface{}
	}{
		{"return 1;", 1},
		{"return x;", "x"},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if len(program.Statements) != 1 {
			t.Fatalf("len(program.Statements) is not 1, got=%d\n",
				len(program.Statements))
		}

		retStmt, ok := program.Statements[0].(*ast.ReturnStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ReturnStatement, got=%T\n",
				program.Statements[0])
		}

		if retStmt.TokenLiteral() != "return" {
			t.Fatalf("retStmt.TokenLiteral() is not 'return', got='%s'\n",
				retStmt.TokenLiteral())
		}

		if !testLiteralExpression(t, retStmt.ReturnValue, tt.returnValue) {
			return
		}

	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	if program == nil {
		t.Fatal("p.ParserProgram() return nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements not contain 1 element, got=%d\n",
			len(program.Statements))
	}

	expression, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("statement can not convert to ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	identifier, ok := expression.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf("expression can not convert to ast.Identifier, got=%T\n",
			expression)
	}

	if identifier.TokenLiteral() != "foobar" {
		t.Errorf("identifier.TokenLiteral() is not 'foobar', got=%q\n",
			identifier.TokenLiteral())
	}

	if identifier.Value != "foobar" {
		t.Errorf("identifier.Value is not 'foobar', got=%s\n",
			identifier.Value)
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	//判断是否为空
	if program == nil {
		t.Fatal("p.ParseProgram() return nil")
	}

	//判断statement的个数是否正确
	if len(program.Statements) != 1 {
		t.Fatalf("len(program.Statements) not 1, got=%d\n",
			len(program.Statements))
	}

	//判断statement是否是表达式
	expr, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	//判断Expression字段是否可以转换为整数表达式类型
	integerLiteral, ok := expr.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("expr.Expression is not ast.IntegerLiteral, got=%T\n",
			expr.Expression)
	}

	if integerLiteral.TokenLiteral() != "5" {
		t.Errorf("integerLiteral.TokenLiteral() is not '5', got=%q\n",
			integerLiteral.TokenLiteral())
	}
	if integerLiteral.Value != 5 {
		t.Errorf("integerLiteral.Value is not 5, got=%d\n",
			integerLiteral.Value)
	}
}

func TestPrefixExpression(t *testing.T) {
	prefixExpressiomTest := []struct {
		input    string
		operator string
		right    interface{}
	}{
		{"-5", "-", 5},
		{"!33", "!", 33},
		{"!true", "!", true},
		{"!false", "!", false},
	}

	for _, tt := range prefixExpressiomTest {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)
		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}
		if len(program.Statements) != 1 {
			t.Fatalf("len(program.Statements) is not 1. got=%d\n",
				len(program.Statements))
		}
		ExprStmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] can't convert to ast.ExpressionStatement, got=%T\n",
				program.Statements[0])
		}

		if !testPrefixExpression(t, ExprStmt.Expression, tt.operator, tt.right) {
			return
		}
	}
}

func testPrefixExpression(t *testing.T, expr ast.Expression, operator string, right interface{}) bool {
	prefixExpr, ok := expr.(*ast.PrefixExpression)
	if !ok {
		t.Errorf("expr is not ast.PrefixExpression, got=%T\n",
			expr)
		return false
	}

	if prefixExpr.Operator != operator {
		t.Errorf("prefixExpr.Operator is not '%s', got='%s'\n",
			operator, prefixExpr.Operator)
	}

	if !testLiteralExpression(t, prefixExpr.Right, right) {
		return false
	}

	return true
}

func testIntegerLiteral(t *testing.T, right ast.Expression, value int64) bool {
	integerLiteral, ok := right.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("right is not ast.IntegerLiteral, got=%T\n",
			right)
		return false
	}
	if integerLiteral.Value != value {
		t.Errorf("integerLiteral.Value is not %d, got=%d\n",
			value, integerLiteral.Value)
		return false
	}
	if integerLiteral.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("integerLiteral.TokenLiteral() is not %s, got=%q\n",
			fmt.Sprintf("%d", value), integerLiteral.TokenLiteral())
		return false
	}
	return true
}

func TestParseInfixExpression(t *testing.T) {
	infixTest := []struct {
		input      string
		leftValue  interface{}
		operator   string
		rightValue interface{}
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
		{"true == true", true, "==", true},
		{"true != false", true, "!=", false},
		{"false == false", false, "==", false},
	}

	for _, tt := range infixTest {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if len(program.Statements) != 1 {
			t.Fatalf("len(program.Statements) is not 1, got=%d\n",
				len(program.Statements))
		}

		exprStmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
				program.Statements[0])
		}

		infixExpr, ok := exprStmt.Expression.(*ast.InfixExpression)
		if !ok {
			t.Fatalf("exprStmt.Expression is not ast.InfixExpression, got=%T\n",
				exprStmt)
		}

		if !testInfixExpression(t, infixExpr, tt.leftValue, tt.operator, tt.rightValue) {
			return
		}
	}
}

func TestOperaroePrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"-a * b",
			"((-a) * b)",
		},
		{
			"!-a",
			"(!(-a))",
		},
		{
			"a + b + c",
			"((a + b) + c)",
		},
		{
			"a + b - c",
			"((a + b) - c)",
		},
		{
			"a * b * c",
			"((a * b) * c)",
		},
		{
			"a * b / c",
			"((a * b) / c)",
		},
		{
			"a + b / c",
			"(a + (b / c))",
		},
		{
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		},
		{
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if program.String() != tt.expected {
			t.Errorf("expected=%s, got=%s\n",
				tt.expected, program.String())
		}
	}
}

func testIdentifier(t *testing.T, exp ast.Expression, value string) bool {
	identifierExpr, ok := exp.(*ast.Identifier)
	if !ok {
		t.Errorf("expression is not ast.Identifier, got=%T\n",
			exp)
		return false
	}

	if identifierExpr.Value != value {
		t.Errorf("identifierExpr.Value is not '%s', got='%s'\n",
			value, identifierExpr.Value)
		return false
	}

	if identifierExpr.TokenLiteral() != value {
		t.Errorf("identifierExpr.TokenLiteral() is not '%s', got='%s'\n",
			value, identifierExpr.TokenLiteral())
		return false
	}

	return true
}

func testLiteralExpression(t *testing.T, exp ast.Expression, value interface{}) bool {
	switch v := value.(type) {
	case int:
		return testIntegerLiteral(t, exp, int64(v))
	case int64:
		return testIntegerLiteral(t, exp, v)
	case string:
		return testIdentifier(t, exp, v)
	case bool:
		return testBooleanLiteral(t, exp, v)
	}
	t.Errorf("can't handle '%T' type\n", value)
	return false
}

func testInfixExpression(t *testing.T, exp ast.Expression, left interface{},
	operator string, right interface{}) bool {
	infixExpression, ok := exp.(*ast.InfixExpression)
	if !ok {
		t.Errorf("expr is not ast.InfixExpression, got=%T\n",
			exp)
		return false
	}

	if !testLiteralExpression(t, infixExpression.Left, left) {
		return false
	}

	if infixExpression.Operator != operator {
		t.Errorf("infixExpression.Operator is not '%s', got='%s'\n",
			operator, infixExpression.Operator)
		return false
	}

	if !testLiteralExpression(t, infixExpression.Right, right) {
		return false
	}

	return true
}

func TestBooleanExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{
			"true;",
			true,
		},
		{
			"false;",
			false,
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if len(program.Statements) != 1 {
			t.Fatalf("len(program.Statements) is not 1, got=%d\n",
				len(program.Statements))
		}

		exprStmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
				program.Statements[0])
		}

		booleanExpr, ok := exprStmt.Expression.(*ast.Boolean)
		if !ok {
			t.Fatalf("exprStmt.Expression is not ast.Boolean, got=%T\n",
				booleanExpr)
		}

		if booleanExpr.Value != tt.expected {
			t.Fatalf("booleanExpr.Value is not '%t', got='%t'",
				tt.expected, booleanExpr.Value)
		}
	}
}

func TestOperatorPrecedenceParsing1(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"true",
			"true",
		},
		{
			"false",
			"false",
		},
		{
			"3 > 5 == false;",
			"((3 > 5) == false)",
		},
		{
			"3 < 5 == true;",
			"((3 < 5) == true)",
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram()")
		}

		if program.String() != tt.expected {
			t.Errorf("program.String() is not '%s', got='%s'\n",
				tt.expected, program.String())
		}
	}
}

func testBooleanLiteral(t *testing.T, expr ast.Expression, value bool) bool {
	booleanExpr, ok := expr.(*ast.Boolean)
	if !ok {
		t.Errorf("expr is not ast.Boolean, got=%T\n",
			expr)
		return false
	}

	if booleanExpr.Value != value {
		t.Errorf("booleanExpr.Value is not %t, got=%t\n",
			value, booleanExpr.Value)
		return false
	}

	if booleanExpr.TokenLiteral() != fmt.Sprintf("%t", value) {
		t.Errorf("booleanExpr.TokenLiteral() is not %s, got=%s\n",
			fmt.Sprintf("%t", value), booleanExpr.TokenLiteral())
		return false
	}

	return true
}

func TestOperatorPrecedenceParsing2(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"1 + (2 + 3) + 4",
			"((1 + (2 + 3)) + 4)",
		},
		{
			"(5 + 5) * 2",
			"((5 + 5) * 2)",
		},
		{
			"2 / (5 + 5)",
			"(2 / (5 + 5))",
		},
		{
			"-(5 + 5)",
			"(-(5 + 5))",
		},
		{
			"!(true == true)",
			"(!(true == true))",
		},
		{
			"a + add(b, c) + d",
			"((a + add(b, c)) + d)",
		},
		{
			"add(a, add(b, c), d)",
			"add(a, add(b, c), d)",
		},
		{
			"add(a + b * c)",
			"add((a + (b * c)))",
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		if program.String() != tt.expected {
			t.Errorf("program.String() is not '%s', got='%s'\n",
				tt.expected, program.String())
		}
	}
}

func TestIfExpression(t *testing.T) {
	input := "if (x < y) { x }"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatal("p.ParseProgram() return nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("len(program.Statements) is not %d, got=%d\n",
			1, len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	ifExpr, ok := stmt.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("stmt.Expression is not ast.IfExpression, got=%T\n",
			stmt.Expression)
	}

	if ifExpr.TokenLiteral() != "if" {
		t.Errorf("ifExpr.TokenLiteral() is not 'if', got='%s'\n",
			ifExpr.TokenLiteral())
	}

	if !testInfixExpression(t, ifExpr.Condition, "x", "<", "y") {
		return
	}

	if ifExpr.Consequence == nil {
		t.Fatalf("ifExpr.Consequence is nil")
	}

	if len(ifExpr.Consequence.Statements) != 1 {
		t.Fatalf("len(ifExpr.Consequence.Statements) is not 1, got=%d\n",
			len(ifExpr.Consequence.Statements))
	}

	exprStmt, ok := ifExpr.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("ifExpr.Consequence.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			ifExpr.Consequence.Statements[0])
	}

	if !testIdentifier(t, exprStmt.Expression, "x") {
		return
	}

	if ifExpr.Alternative != nil {
		t.Errorf("ifExpr.Alternative is not nil, got=%+v\n",
			ifExpr.Alternative)
	}
}

func TestIfElseExpression(t *testing.T) {
	input := "if (x < y) { x } else { y }"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatal("p.ParseProgram() return nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("len(program.Statements) is not %d, got=%d\n",
			1, len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	ifExpr, ok := stmt.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("stmt.Expression is not ast.IfExpression, got=%T\n",
			stmt.Expression)
	}

	if ifExpr.TokenLiteral() != "if" {
		t.Errorf("ifExpr.TokenLiteral() is not 'if', got='%s'\n",
			ifExpr.TokenLiteral())
	}

	if !testInfixExpression(t, ifExpr.Condition, "x", "<", "y") {
		return
	}

	if ifExpr.Consequence == nil {
		t.Fatalf("ifExpr.Consequence is nil")
	}

	if len(ifExpr.Consequence.Statements) != 1 {
		t.Fatalf("len(ifExpr.Consequence.Statements) is not 1, got=%d\n",
			len(ifExpr.Consequence.Statements))
	}

	condiexprStmt, ok := ifExpr.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("ifExpr.Consequence.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			ifExpr.Consequence.Statements[0])
	}

	if !testIdentifier(t, condiexprStmt.Expression, "x") {
		return
	}

	if ifExpr.Alternative == nil {
		t.Error("ifExpr.Alternative is  nil")
	}

	if len(ifExpr.Alternative.Statements) != 1 {
		t.Fatalf("len(ifExpr.Alternative.Statements) is not 1, got=%T\n",
			len(ifExpr.Alternative.Statements))
	}

	alterexprStmt, ok := ifExpr.Alternative.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("ifExpr.Alternative.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			ifExpr.Alternative.Statements[0])
	}

	if !testIdentifier(t, alterexprStmt.Expression, "y") {
		return
	}
}

func TestFunctionLiteral(t *testing.T) {
	input := `
		fn(x, y) {
			x + y;
		}
	`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatal("p.ParseProgram() return nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("len(program.Statements) is not 1, got=%d\n",
			len(program.Statements))
	}

	stmt1, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	functionLiteral, ok := stmt1.Expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("stmt1.Expression is  not ast.FunctionLiteral, got=%T\n",
			stmt1.Expression)
	}

	if len(functionLiteral.Params) != 2 {
		t.Fatalf("len(functionLiteral.Params) is not 2, got=%d\n",
			len(functionLiteral.Params))
	}

	testLiteralExpression(t, functionLiteral.Params[0], "x")
	testLiteralExpression(t, functionLiteral.Params[1], "y")

	if functionLiteral.Body == nil {
		t.Fatal("functionLiteral.Body is nil")
	}

	body := functionLiteral.Body
	if len(body.Statements) != 1 {
		t.Fatalf("len(body.Statements) is not 1, got=%d\n",
			len(body.Statements))
	}

	stmt2, ok := body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("body.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			body.Statements[0])
	}

	infixExpr, ok := stmt2.Expression.(*ast.InfixExpression)
	if !ok {
		t.Fatalf("stmt2.Expression is not ast.InfixExpression")
	}

	testInfixExpression(t, infixExpr, "x", "+", "y")
}

func TestFunctionParamParsing(t *testing.T) {
	tests := []struct {
		input          string
		expectedParams []string
	}{
		{"fn(){}", []string{}},
		{"fn(x){}", []string{"x"}},
		{"fn(x, y, z){}", []string{"x", "y", "z"}},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if program == nil {
			t.Fatal("p.ParseProgram() return nil")
		}

		stmt, _ := program.Statements[0].(*ast.ExpressionStatement)
		function, _ := stmt.Expression.(*ast.FunctionLiteral)

		if len(function.Params) != len(tt.expectedParams) {
			t.Fatalf("len(function.Params) is not %d, got=%d\n",
				len(tt.expectedParams), len(function.Params))
		}

		for i, p := range tt.expectedParams {
			testLiteralExpression(t, function.Params[i], p)
		}

	}
}

func TestCallExpressionParsing(t *testing.T) {
	input := "add(1, 2+3, 4*5);"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatal("p.ParseProgram() return nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf("len(program.Statements) is not 1, got=%d\n",
			len(program.Statements))
	}

	exprStmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T\n",
			program.Statements[0])
	}

	callExpr, ok := exprStmt.Expression.(*ast.CallExpression)
	if !ok {
		t.Fatalf("exprStmt.Expression is not ast.CallExpression, got=%T\n",
			exprStmt.Expression)
	}

	if !testIdentifier(t, callExpr.Function, "add") {
		return
	}

	if len(callExpr.Params) != 3 {
		t.Fatalf("len(callExpr.Params) is not 3, got=%d\n",
			len(callExpr.Params))
	}

	testLiteralExpression(t, callExpr.Params[0], 1)
	testInfixExpression(t, callExpr.Params[1], 2, "+", 3)
	testInfixExpression(t, callExpr.Params[2], 4, "*", 5)
}
