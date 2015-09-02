/++
    simpleread.d is a temporary module to eventually replace getTokens
    function from parser.d.

    The first is implementing Token struct type to structurize better
    organization for token. It can be extended by importing this module
    and defining new token character.

    Authors: Mashingan, https://github.com/mashingan
    Bugs: Haven't the detection validity comma and quasiquote
    Date: August 8, 2015
+/
import std.stdio;
import std.ascii;

template CharType (U) {
    enum bool CharType = (is (U == char[]) || is (U == string));
}

struct Token(char val) {
    char start = val;

    bool opEquals(T) (auto ref const T rhs) {
        char theval;
        static if (CharType!T)
            theval = rhs[0];
        else static if (is (T == char))
            theval = rhs;
        return start == theval;
    }
}

enum SPECIAL_CHARS = "\"'`,(";

Token!('"') stringToken;
Token!('\'') quoteToken;
Token!('`') quasiToken;
Token!(',') commaToken;
Token!('(') exprToken;
Token!('@') expandToken;

alias StringToken     = Token!('"');
alias QuasiToken      = Token!('`');
alias QuoteToken      = Token!('\'');
alias CommaToken      = Token!(',');
alias ExpressionToken = Token!('(');

string extractInit() {
    return "U res; res ~= line[0]; line = line[1..$];";
}

string extractInit2() {
    return "
        bool more = line.length > 0 ? true : false;
        char c;
        if (more) c = line[0];
    ";
}

U extract(U)(ExpressionToken dummy, ref U line) if (CharType!U) {
    assert(line[0] == exprToken);
    mixin(extractInit());
    int start = 1;
    int end = 0;
    bool spaced = false;
    while (line && start != end) {
        if (line.length == 0 && start != end)
            throw new Error("Inbalace expression");

        auto c = line[0];
        if (c == '(')
            ++start;
        else if (c == ')')
            ++end;

        if (c == '\n') {
            line = line[1..$];
            continue;
        } else if (isWhite(c) && spaced) {
            line = line[1..$];
            continue;
        } else if (isWhite(c))
            spaced = true;
        else if (!isWhite(c))
            spaced = false;

        res ~= c;
        line = line[1..$];
    }
    return res;
}

U otherTokens(U) (ref U line) if (CharType!U) {
    U res;
    mixin(extractInit2());
    if (c && c == exprToken)
        res ~= extract(exprToken, line);
    else if (c && c == quoteToken)
        res ~= extract(quoteToken, line);
    else if (c && c == stringToken)
        res ~= extract(stringToken, line);
    else if (c && c == quasiToken)
        res ~= extract(quasiToken, line);
    else if (c && c == commaToken)
        res ~= extract(commaToken, line);
    else
        res ~= extract(line);
    return res;
}

U extract(U)(QuoteToken dummy, ref U line) if (CharType!U) {
    assert(line[0] == quoteToken);
    mixin(extractInit());
    mixin(extractInit2());
    res ~= otherTokens(line);
    return res;
}

U extract(U)(StringToken dummy, ref U line) if (CharType!U) {
    assert(line[0] == stringToken);
    mixin(extractInit());
    while (line.length > 0) {
        auto c = line[0];

        if (c == '\\' && line.length > 1 && line[1] == '"') {
            res ~= line[0..2];
            line = line[0..2];
            continue;
        }

        res ~= c;
        line = line[1..$];
        if (c == '"')
            break;

    }
    return res;
}

U extract(U)(QuasiToken dummy, ref U line) if (CharType!U) {
    assert(line[0] == quasiToken);
    mixin(extractInit());
    mixin(extractInit2());
    res ~= otherTokens(line);
    return res;
}

U extract(U)(CommaToken dummy, ref U line) if (CharType!U) {
    assert(line[0] == commaToken);
    mixin(extractInit());
    mixin(extractInit2());
    res ~= otherTokens(line);
    return res;
}

U extract(U)(ref U line) if (CharType!U) {
    U res;
    while (line.length > 0) {
        auto isMoreThan1 = line.length > 1 ? true : false;
        auto c = line[0];
        if (isWhite(c) || c == '(')
            break;

        if (isMoreThan1 && (line[0..2] == "@(" || line[0..2] == "&(")) {
            res ~= c;
            line = line[1..$];
            res ~= extract(exprToken, line);
            continue;
        }

        if (line == "\n" || line == "\r\n" || line == "\n\r") {
            res = "";
            break;
        }

        res ~= c;
        line = line[1..$];
    }
    return res;
}

U getExpr(U) (ref U line) if (CharType!U) {
    U tokens;
    mixin(extractInit2());

    if (c == exprToken)
        tokens ~= extract(exprToken, line);
    else
        tokens ~= otherTokens(line);

    return tokens;
}

U[] getTokens(U) (U line) if (CharType!U) {
    U[] tokens;

    auto expr = getExpr(line);
    if (expr == exprToken) {
        debug writeln("getTokens: exprToken: ", expr);
        expr = expr.length > 0 ? expr[1..$-1] : expr;
        while (expr.length > 0) {
            auto c = expr[0];
            if (isWhite(c)) {
                expr = expr[1..$];
                continue;
            }
            debug writeln("now expr: ", expr);
            tokens ~= getExpr(expr);
        }
    } else
        tokens ~= otherTokens(expr);

    return tokens;
}
