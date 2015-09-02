import std.stdio;
import std.ascii;
import std.conv;
import std.variant;

import simpleread;
import simpledatatypes;

enum LVAL
{
    SYMBOL, STRING, QUOTE,
    LIST, PAIR, CHAR,
    QUASIQUOTE, COMMA,
    NULL, ERR, FUNCTION, LAMBDA,
    NUMBER, DOUBLE, INT, FLOAT, LONG, FRACTION
}

string getLine() {
    int lstart, lend = 0;
    bool dquote_started = false;

    bool sameParenthesis(string ln) {
        foreach (int idx, c; ln) {
            char before = idx != 0 ? ln[idx-1] : char.init;
            if (c == '"' && !dquote_started && before != '\\')
                dquote_started = true;
            else if (c == '"' && before != '\\')
                dquote_started = false;

            if (c == '(' && !dquote_started) lstart++;
            else if (c == ')' && !dquote_started) lend++;

        }
        return (lstart == lend && !dquote_started) ? true : false;
    }

    string line;
    string expression;

    do {
        line = readln();
        expression ~= line;
    } while (!sameParenthesis(line));

    return expression;
}

LVAL identify (U) (U token) if (CharType!U) {
    auto isChars = token.length > 1 ? true : false;

    LVAL result;
    import std.string;
    if (token[0] == exprToken) {
        result = LVAL.LIST;

    } else if (token[0] == quoteToken) {
        result = LVAL.QUOTE;

    } else if (token[0] == quasiToken) {
        result = LVAL.QUASIQUOTE;

    } else if (token[0] == commaToken) {
        result = LVAL.COMMA;

    } else if (token[0] == stringToken) {
        result = LVAL.STRING;

    } else if (isNumeric(token)) {
        if ((indexOf(token, '.') != -1) ||
                (indexOf(token, 'e', CaseSensitive.no) != -1))
            result = LVAL.DOUBLE;
        else
            result = LVAL.LONG;

    } else if (token == "nil" || token == "()" || token == "'()") {
        result = LVAL.NULL;

    } else if (isChars && token[0 .. 2] == "#\\") {
        result = LVAL.CHAR;

    } else if (isChars && token[0 .. 2] == "#'") {
        result = LVAL.FUNCTION;

    } else if (!isWhite(token[0])) {
        result = LVAL.SYMBOL;

    } else {
        result = LVAL.ERR;
    }

    return result;
}

LispObject[] readall(T)(T[] tokens) if (CharType!T) {
    LispObject[] objs;
    foreach (token; tokens)
        objs ~= read(token);
    return objs;
}

import std.string : format;
string processQuote (string mark) {
    return format(`
            T[] tokens;
            LispObject[] objs;
            LispObject res;
            auto moreChars = token.length > 1 ? true : false;
            if (moreChars && token[0..2] == "%s") {
                tokens = getTokens(token[1..$]);
                objs = readall(tokens);
                res = LispObject(Variant(null), lval, objs);
            } else {
                token = getExpr(token);
                res = LispObject(Variant(token[1..$]), lval);
            }
            return res;
            `, mark);
}

LispObject read(T) (T token) if (CharType!T) {
    LVAL lval = identify(token);
    Variant val;
    switch (lval) {
        case LVAL.INT:
            val = to!int(token);
            break;

        case LVAL.DOUBLE:
            val = to!double(token);
            break;

        case LVAL.NUMBER:
        case LVAL.LONG:
            val = to!long(token);
            break;

        case LVAL.STRING:
            val = token;
            break;

        case LVAL.LIST:
            auto tokens = getTokens(token);
            auto objs = readall(tokens);
            LVAL outlval;
            if (tokens.length == 0)
                return LispObject(Variant(null), LVAL.NULL, objs);

            if (tokens[0] == "lambda")
                outlval = LVAL.LAMBDA;
            else if (tokens[0] == "quote")
                outlval = LVAL.QUOTE;
            else
                outlval = lval;
            return LispObject(Variant(null), outlval, objs);
            break;

        case LVAL.QUOTE:
            mixin(processQuote("'("));
            break;

        case LVAL.QUASIQUOTE:
            mixin(processQuote("`("));
            break;

        case LVAL.COMMA:
            mixin(processQuote(",("));
            break;

        case LVAL.FUNCTION:
            val = token[2..$];
            break;

        case LVAL.CHAR:
        default:
            val = token;
    }
    return LispObject(val, lval);
}
