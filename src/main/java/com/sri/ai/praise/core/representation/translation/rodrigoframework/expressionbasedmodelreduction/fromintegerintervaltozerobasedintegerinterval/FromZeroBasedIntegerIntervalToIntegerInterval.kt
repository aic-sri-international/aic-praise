package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.makeSymbol
import com.sri.ai.expresso.type.IntegerInterval
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.base.AbstractExpressionBasedModelUpReduction

/**
 * Given an {@link ExpressionBasedModel} containing categorical variables,
 * translates expressions with such variables converted to integers back to their categorical counterparts.
 * It assumes the sub-expression containing such variables to be in the simple form <code>variable = value</code>
 */
class FromZeroBasedIntegerIntervalToIntegerInterval(
        upperExpressionBasedModel: ExpressionBasedModel,
        lowerExpressionBasedModel: ExpressionBasedModel
        ) : AbstractExpressionBasedModelUpReduction(upperExpressionBasedModel, lowerExpressionBasedModel) {

    override fun translateSubExpressionBack(expression: Expression?): Expression =
            if (expression!!.hasFunctor(FunctorConstants.EQUALITY)) {
                translateEqualityBack(expression)
            }
            else if (upperExpressionBasedModel.context.isVariable(expression)) {
                val type = upperExpressionBasedModel.context.getTypeOfRegisteredSymbol(expression)
                if (type is IntegerInterval && type.nonStrictLowerBound != Expressions.ZERO) {
                    Expressions.apply(FunctorConstants.MINUS, expression, type.nonStrictLowerBound)
                }
                else {
                    expression
                }
            }
            else {
                expression
            }

    private fun translateEqualityBack(equality: Expression): Expression {
        return when {
            upperExpressionBasedModel.context.isVariable(equality.get(0)) -> {
                translateEqualityOnVariableBack(equality, equality.get(0), 0)
            }
            upperExpressionBasedModel.context.isVariable(equality.get(1)) -> {
                translateEqualityOnVariableBack(equality, equality.get(1), 1)
            }
            else -> {
                equality
            }
        }
    }

    private fun translateEqualityOnVariableBack(
            equalityOnVariable: Expression,
            variable: Expression,
            variableIndex: Int): Expression {

        val type = GrinderUtil.getTypeOfExpression(variable, upperExpressionBasedModel.context)
        return if (type is IntegerInterval) {
            translateIntegerEqualityBack(equalityOnVariable, variable, variableIndex, type)
        } else {
            equalityOnVariable
        }
    }

    private fun translateIntegerEqualityBack(
            equality: Expression,
            variable: Expression,
            variableIndex: Int,
            type: IntegerInterval): Expression {

        val constant = getConstant(equality, variableIndex)
        return translateDecomposedIntegerEqualityBack(variable, constant, variableIndex, type)
    }

    private fun getConstant(equality: Expression, variableIndex: Int): Expression {
        val constantIndex = 1 - variableIndex
        return equality.get(constantIndex)
    }

    private fun translateDecomposedIntegerEqualityBack(
            variable: Expression,
            constant: Expression,
            variableIndex: Int,
            type: IntegerInterval): Expression {

        val nonZeroBasedConstant = makeSymbol(constant.intValue() + type.nonStrictLowerBound.intValue())
        return if (variableIndex == 0) {
            Expressions.apply(FunctorConstants.EQUALITY, variable, nonZeroBasedConstant)
        } else {
            Expressions.apply(FunctorConstants.EQUALITY, nonZeroBasedConstant, variable)
        }
    }

}