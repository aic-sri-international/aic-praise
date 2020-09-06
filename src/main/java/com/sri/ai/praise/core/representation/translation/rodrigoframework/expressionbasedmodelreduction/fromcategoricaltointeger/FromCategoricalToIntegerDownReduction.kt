package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.*
import com.sri.ai.expresso.type.Categorical
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants.EQUALITY
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.base.AbstractExpressionBasedModelDownReduction

class FromCategoricalToIntegerDownReduction(expressionBasedModel: ExpressionBasedModel)
    : AbstractExpressionBasedModelDownReduction(expressionBasedModel) {

    override fun processTypeName(typeName: String) = replaceCategoricalTypeNameByIntegerIntervalTypeName(typeName)

    private fun replaceCategoricalTypeNameByIntegerIntervalTypeName(typeName: String): String {
        val type = expressionBasedModel.context.getType(typeName)
        return if (type is Categorical) {
            "0..${type.cardinality().intValue() - 1}"
        } else {
            typeName
        }
    }

    override fun processSubExpression(subExpression: Expression) = replaceCategoricalConstants(subExpression)

    private fun replaceCategoricalConstants(subExpression: Expression): Expression {
        return categoricalConstantTranslation.getOrElse(subExpression) {
            val newTranslation = translateExpression(subExpression)
            if (newTranslation != subExpression) {
                categoricalConstantTranslation[subExpression] = newTranslation
                newTranslation
            } else {
                subExpression
            }
        }
    }

    private val categoricalConstantTranslation: MutableMap<Expression, Expression> by lazy {
        var categoricalConstantTranslation = mutableMapOf<Expression, Expression>()
        expressionBasedModel.context.types
                .filterIsInstance<Categorical>()
                .forEach { categorical ->
                    categorical.knownConstants.forEach { known ->
                        if (categorical.name == "Boolean") {
                            categoricalConstantTranslation[known] = known
                            // We leave boolean constants untranslated because
                            // we assume the target interpreter can manage them.
                            // This is reasonable because it must be able to interpret boolean values anyway
                            // (to process boolean-valued expressions such as X = 1)
                            // Note that we still do not assume it can handle boolean *variables*
                            // because all variables must be converted to integer types.
                            // If all variables must be converted to integer type, one may ask what
                            // happens to random p: Boolean; if p = false then 1 else 0
                            // since 'false' will remain Boolean.
                            // This will be translated into if (p = 1) = false then 1 else 0,
                            // because boolean variables must always be converted into equalities with a constant
                            // integer (in case they are alone in a condition such as if p then 1 else 0,
                            // which requires a translation if p = 1 then 1 else 0).
                            // It would be much more complicated to write code to translate
                            // if p then 1 else 0 to if p = 1 then 1 else 0
                            // while at the same time translating
                            // if p = true then 1 else 0 to if p = 1 then 1 else 0
                            // because this would require detecting whether p is used by itself or in a comparison
                            // in arbitrary functions.
                        }
                        else {
                            categoricalConstantTranslation[known] = integerCodeExpression(categorical, known)
                        }
                    }
                }
        categoricalConstantTranslation
    }

    private fun translateExpression(expression: Expression): Expression {
        return if (isTranslatableSymbol(expression)) {
            translateSymbol(expression)
        } else {
            expression
        }
    }

    private fun translateSymbol(expression: Expression): Expression {
        val type = GrinderUtil.getTypeOfExpression(expression, expressionBasedModel.context)
        return if (type is Categorical) {
            translateCategoricalSymbol(expression, type)
        } else {
            expression
        }
    }

    private fun translateCategoricalSymbol(expression: Expression, type: Categorical): Expression {
        return when {
            type.name == "Boolean" -> {
                // we know here expression is a variable because Boolean constants
                // are always found in categoricalConstantTranslation.
                // We must replace a Boolean variable p by p = <integer constant of true>
                // because it may be in a boolean condition such as if p then 2 else 3
                // and leaving it as-is would not be legal when it is converted to an integer.
                Expressions.apply(EQUALITY, expression, integerCodeExpression(type, TRUE))
            }
            !expressionBasedModel.context.isVariable(expression) -> {
                integerCodeExpression(type, expression)
            }
            else -> {
                expression
            }
        }
    }

    private fun isTranslatableSymbol(expression: Expression): Boolean {
        return expression.syntacticFormType == "Symbol"
                &&
                !isInterpretedSymbolOtherThanTrueAndFalse(expression)
    }

    private fun isInterpretedSymbolOtherThanTrueAndFalse(expression: Expression): Boolean {
        return expression != TRUE
                &&
                expression != FALSE
                &&
                expressionBasedModel.context.theory.expressionKnownToBeSymbolIsInterpretedInThisTheory(expression)
    }

    private fun integerCodeExpression(categoricalType: Categorical, knownConstant: Expression?) =
            if (categoricalType.name == "Boolean") {
                if (knownConstant == TRUE) ONE else ZERO
            }
            else {
                makeSymbol(categoricalType.indexOfConstant(knownConstant))
            }

}