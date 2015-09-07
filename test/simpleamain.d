import std.stdio;

import simpleread;
import simpleext01;

string report() {
    return "
        auto tokens = getExpr(expr);
        writeln(expr);
        writeln(line);
        writeln(tokens);
        ";
}

void main() {
    assert(stringToken == '"');
    assert(quoteToken == '\'');
    assert(quasiToken == '`');
    assert(commaToken == ',');
    assert(exprToken == '(');
    {
        writeln("---------\nexpression:");
        //auto line = "(hello world) oh nice";
        auto line = "(,hello ,(world)) oh nice";
        //auto line = "(hello '(really (really)) world oh nice)";
        auto expr = extract(exprToken, line);
        mixin(report());
    }
    {
        writeln("---------\nstring:");
        auto line = "\"hello my dear,
oh god I've been newlined oh really? (\nreally\"what happen to you?";
        auto expr = extract(stringToken, line);
        mixin(report());
    }
    {
        writeln("---------\nquote:");
        //auto line = "'hello(world oh nice)";
        //auto line = "'(hello world) oh nice";
        auto line = "'abc(1 2 (3 4) 5)";
        auto expr = extract(quoteToken, line);
        mixin(report());
    }
    {
        writeln("---------\nquasiquote:");
        //auto line = "`hello world oh nice";
        //auto line = "`(hello world) oh nice";
        //auto line = "`'(hello world) oh nice";
        //auto line = "`,'(hello world) oh nice";
        auto line = "`(,hello ,(world)) oh nice";
        auto expr = extract(quasiToken, line);
        mixin(report());
    }
    {
        writeln("---------\ncomma:");
        //auto line = ",hello world oh nice";
        //auto line = ",(hello world) oh nice";
        //auto line = ",'(hello world) oh nice";
        auto line = ",',(hello world) oh nice";
        auto expr = extract(commaToken, line);
        mixin(report());
    }
    {
        writeln("---------\nreadall:");
        auto line = "(defun map (fun lst)
            (if (null lst) '()
             (cons (fun (car lst)) (map (cdr lst)))))";
        writeln("The tokens: ", getExpr(line));
        debug writeln("Now the line is ",
                line.length > 0 ? line : "no line");
    }
    {
        auto line = ",(hello) world ok";
        writeln(getExpr(line));
    }
}
