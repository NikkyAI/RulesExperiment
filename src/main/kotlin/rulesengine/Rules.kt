package rulesengine

/**
 * Created by nikky on 01/02/18.
 * @author Nikky
 * @version 1.0
 */

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.parser.Parser
import rulesengine.RulesGrammar.getValue
import rulesengine.RulesGrammar.provideDelegate
import java.io.File

data class RuleLanguage(
        val tags: List<TagStatement>,
        val rules: List<RuleStatement>
)

sealed class Expression

data class Variable(val name: String) : Expression()
data class Not(var body: Expression) : Expression()
data class And(var left: Expression, var right: Expression) : Expression()
data class Or(var left: Expression, var right: Expression) : Expression()
data class Const(val prefix: String, val value: String) : Expression()
data class Ref(var name: String, var value: Expression) : Expression() {
    override fun toString(): String {
        return "Ref(ref=$name)"
    }
}
//data class BoolConst(val value: Boolean) : Expression()

sealed class Statement
data class TagStatement(
        val name: String,
        var value: Expression
) : Statement()

data class RuleStatement(
        val weight: Int,
        var condition: Expression,
        val effects: List<Effect>,
        var label: String = ""
) : Statement()

data class Effect(val name: String)

object RulesGrammar : Grammar<RuleLanguage>() {
    //    val TRUE by token("true")
//    val FALSE by token("false")
    private val NUMBER by token("\\d+") // must be before ID

    private val PREFIX_ID by token("[$%&§]\\w+") // must be before AND / OR
    private val ID by token("\\w+")

    private val LPAR by token("\\(")
    private val RPAR by token("\\)")
    private val NOT by token("!")
    private val AND by token("&")
    private val OR by token("\\|")


    private val SEMI by token(";")

    private val WHITESPACE by token("\\s+", ignore = true)
    private val NEWLINE by token("[\r\n]+", ignore = true)
    private val COMMENT by token("#[^\r\n]+", ignore = true)

    private val EQ by token("=")
    private val IMPL by token("->")

    val notTerm by -NOT * parser(this::term) map { Not(it) }

    val variable: Parser<Expression> by ID map { Variable(name = it.text) }
    val const: Parser<Expression> by
//    (TRUE asJust BoolConst(true)) or
//            (FALSE asJust BoolConst(false)) or
    PREFIX_ID map { Const(it.text.substring(0, 1), it.text.substring(1)) }

    private val parenTerm = -LPAR * parser(this::expr) * -RPAR

    private val term: Parser<Expression> = const or variable or notTerm or parenTerm

    private val andChain by leftAssociative(term, AND) { a, _, b -> And(a, b) }
    private val orChain by leftAssociative(andChain, OR) { a, _, b -> Or(a, b) }

    private val expr: Parser<Expression> = orChain

    //private val variable = ID use { Variable(text) }
    private val effect = ID use { Effect(text) }

    // booleanExpr -> effects
    private val ruleStatement: Parser<RuleStatement> =
            ((NUMBER use { text.toInt() }) * expr * -IMPL * oneOrMore(effect) * optional(COMMENT use {text.substring(1)}))
                    .map { (w, v, e,c) -> RuleStatement(w, v, e, c ?: "") }

    // variable = booleanExpr
    private val tagStatement: Parser<TagStatement> =
            ((ID use { text }) * -EQ * expr)
                    .map { (v, e) -> TagStatement(v, e) }

    override val rootParser: Parser<RuleLanguage> = oneOrMore(ruleStatement or tagStatement).map {
        val tags = it.filterIsInstance<TagStatement>()
        val rules = it.filterIsInstance<RuleStatement>()
        RuleLanguage(tags = tags, rules = rules)
    }
}

fun RuleLanguage.resolveVariables() {
    val resolvedTags = mutableListOf<TagStatement>()
    val unknowns = mutableListOf<Variable>()
    val constants = mutableSetOf<Const>()
    for (tag in tags) {
        if (resolvedTags.contains(tag))
            continue
        val unknownList = mutableListOf<Variable>()
        tag.value = tag.value.resolveVariables(
                tags - tag,
                unknownList, constants,
                listOf(tag.name)
        )
        unknowns.addAll(unknownList)

        if (unknownList.isNotEmpty()) {
            println("${tag.name} could not find ${unknownList.map { it.name }}")
        }
        val self = unknownList.find { it.name == tag.name }
        if (self != null) {
            throw Exception("'${self.name}' causes cyclic dependency")
        }
        resolvedTags += tag
    }

    for (rule in rules) {
        val unknownList = mutableListOf<Variable>()
        if(rule.label.isBlank()) rule.label = "${rule.weight} ${rule.condition}"
        rule.condition = rule.condition.resolveVariables(resolvedTags, unknownList, constants, listOf())
        if (unknownList.isNotEmpty()) {
            println("${rule.label} could not find ${unknownList.map { it.name }}")
        }
        unknowns.addAll(unknownList)
    }
    println(unknowns.toIndentString())
    println(constants.toIndentString())
}

fun Expression.resolveVariables(
        tags: List<TagStatement>,
        unknowns: MutableList<Variable>,
        constants: MutableSet<Const>,
        refs: List<String>
): Expression {
    when (this) {
        is And -> {
            left = left.resolveVariables(tags, unknowns, constants, refs)
            right = right.resolveVariables(tags, unknowns, constants, refs)
            return this
        }
        is Or -> {
            left = left.resolveVariables(tags, unknowns, constants, refs)
            right = right.resolveVariables(tags, unknowns, constants, refs)
            return this
        }
        is Not -> {
            body = body.resolveVariables(tags, unknowns, constants, refs)
            return this
        }
        is Const -> {
            constants += this
            return this
        }
        is Ref -> {
            if (!refs.contains(name)) {
                value = value.resolveVariables(tags, unknowns, constants, refs + name)
            } else {
                println("cyclic dependency $name")
                unknowns += Variable(name)
            }
            return this
        }
        is Variable -> {
            val tag = tags.find { it.name == this.name }
            if (tag == null) {
                unknowns += this
                return this // throw NoSuchElementException("unresolved tag $name")
            }
            val value = tag.value.resolveVariables(tags, unknowns, constants, refs + name)
            return Ref(tag.name, value)
        }
    }
}

fun main(args: Array<String>) {
    val expr = """
a = %a & (&b1 | &c1) | &a1 & !&b | !(a1 & &a2)
false = §a & !§a
b = %a | false # fake
c = d & b
d = abc | b
cond = !(a1 & &a2) | a
6 a | b -> asdf
5 (&b1 | &c1) -> effect1 effect2 #somelabel
4 !cond -> effect1 effect2
4 cond -> effect1 effect2
0 abc -> effect1 effect2
"""
    val parsed = RulesGrammar.parseToEnd(expr)

    val parsedString = parsed.toIndentString()

    println("parsed successfully (valid grammar)")
    //println(parsedString)
    File("rules.out").writeText(parsedString)

    println("resolve Variables")

    parsed.resolveVariables()
    val resolvedString = parsed.toIndentString()

    File("rules.resolved.out").writeText(resolvedString)
    //println(resolvedString)

    val parsedCount = parsedString.lines().count()
    val resolvedCount = resolvedString.lines().count()

    println()
    println("$parsedCount -> $resolvedCount")
    println()
    println("labels")
    parsed.rules.map { it.label }.forEach { println(it) }
}


fun Any?.toIndentString(): String {
    val notFancy = toString()
    return buildString(notFancy.length) {
        var indent = 0
        fun StringBuilder.line() {
            appendln()
            repeat(2 * indent) { append(' ') }
        }

        for (char in notFancy) {
            if (char == ' ') continue

            when (char) {
                ')', ']' -> {
                    indent--
                    line()
                }
            }

            if (char == '=') append(' ')
            append(char)
            if (char == '=') append(' ')

            when (char) {
                '(', '[', ',' -> {
                    if (char != ',') indent++
                    line()
                }
            }
        }
    }
}