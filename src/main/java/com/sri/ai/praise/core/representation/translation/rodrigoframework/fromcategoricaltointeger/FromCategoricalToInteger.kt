package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromcategoricaltointeger

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.TRUE
import com.sri.ai.expresso.helper.Expressions.FALSE
import com.sri.ai.expresso.helper.Expressions.ONE
import com.sri.ai.expresso.helper.Expressions.ZERO
import com.sri.ai.expresso.type.Categorical
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants.EQUALITY
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel
import com.sri.ai.util.Util.getValuePossiblyCreatingIt

class FromCategoricalToInteger(val expressionBasedModel: ExpressionBasedModel) {

    val translation: ExpressionBasedModel by lazy {

        val factors = expressionBasedModel.factors.map(::replaceAllCategoricalConstants)

        val mapFromRandomVariableNameToTypeName =
                expressionBasedModel
                        .mapFromRandomVariableNameToTypeName
                        .mapValues(::replaceCategoricalTypeNameByIntegerIntervalTypeName)

        val mapFromNonUniquelyNamedConstantNameToTypeName =
                expressionBasedModel
                        .mapFromNonUniquelyNamedConstantNameToTypeName
                        .mapValues(::replaceCategoricalTypeNameByIntegerIntervalTypeName)

        val mapFromUniquelyNamedConstantNameToTypeName =
                expressionBasedModel
                        .mapFromUniquelyNamedConstantNameToTypeName
                        .mapValues(::replaceCategoricalTypeNameByIntegerIntervalTypeName)

        val mapFromCategoricalTypeNameToSizeString = mapOf<String, String>()

        DefaultExpressionBasedModel(
                factors,
                mapFromRandomVariableNameToTypeName,
                mapFromNonUniquelyNamedConstantNameToTypeName,
                mapFromUniquelyNamedConstantNameToTypeName,
                mapFromCategoricalTypeNameToSizeString,
                expressionBasedModel.additionalTypes,
                expressionBasedModel.isKnownToBeBayesianNetwork,
                expressionBasedModel.theory
        )
    }

    private fun replaceCategoricalTypeNameByIntegerIntervalTypeName(entry: Map.Entry<String, String>): String {
        val type = expressionBasedModel.context.getType(entry.value)
        return if (type is Categorical) {
            "0..${type.cardinality().intValue() - 1}"
        } else {
            entry.value
        }
    }

    private val categoricalConstantTranslation: Map<Expression, Expression> by lazy {
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

    private fun replaceAllCategoricalConstants(expression: Expression): Expression {
        return expression.replaceAllOccurrences({replaceCategoricalConstants(it!!)}, expressionBasedModel.context)
    }

    private fun replaceCategoricalConstants(expression: Expression): Expression {
        return getValuePossiblyCreatingIt(categoricalConstantTranslation, expression, ::translateExpression)
    }

    private fun translateExpression(expression: Expression?): Expression? {
        var nonNullExpression = expression!!
        return if (isTranslatableSymbol(nonNullExpression)) {
            translateSymbol(nonNullExpression)
        } else {
            expression
        }
    }

    private fun translateSymbol(expression: Expression): Expression? {
        val type = GrinderUtil.getTypeOfExpression(expression, expressionBasedModel.context)
        return if (type is Categorical) {
            translateCategoricalSymbol(expression, type)
        } else {
            expression
        }
    }

    private fun translateCategoricalSymbol(expression: Expression, type: Categorical): Expression? {
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
                Expressions.makeSymbol(categoricalType.indexOfConstant(knownConstant))
            }

}