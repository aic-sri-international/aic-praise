package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.ONE
import com.sri.ai.expresso.type.Categorical
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.grinder.library.boole.Not
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.base.AbstractExpressionBasedModelUpReduction

/**
 * Given an {@link ExpressionBasedModel} containing categorical variables,
 * translates expressions with such variables converted to integers back to their categorical counterparts.
 * It assumes the sub-expression containing such variables to be in the simple form <code>variable = value</code>
 */
class FromIntegerToCategoricalUpReduction(
        private val categoricalExpressionBasedModel: ExpressionBasedModel,
        integerExpressionBasedModel: ExpressionBasedModel
        ) : AbstractExpressionBasedModelUpReduction(categoricalExpressionBasedModel, integerExpressionBasedModel) {

    override fun translateSubExpressionBack(expression: Expression?) =
            if (expression!!.hasFunctor(FunctorConstants.EQUALITY)) {
                translateEqualityBack(expression)
            } else {
                expression
            }

    private fun translateEqualityBack(equality: Expression): Expression {
        return when {
            categoricalExpressionBasedModel.context.isVariable(equality.get(0)) -> {
                translateEqualityOnVariableBack(equality, equality.get(0), 0)
            }
            categoricalExpressionBasedModel.context.isVariable(equality.get(1)) -> {
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

        val type = GrinderUtil.getTypeOfExpression(variable, categoricalExpressionBasedModel.context)
        return if (type is Categorical) {
            translateCategoricalEqualityBack(equalityOnVariable, variable, variableIndex, type)
        } else {
            equalityOnVariable
        }
    }

    private fun translateCategoricalEqualityBack(
            expression: Expression,
            variable: Expression,
            variableIndex: Int,
            type: Categorical): Expression {

        val constant = getConstant(expression, variableIndex)
        return if (type.name == "Boolean") {
            translateBooleanEqualityBack(variable, constant)
        } else {
            translateNonBooleanCategoricalEqualityBack(variable, constant, variableIndex, type)
        }
    }

    private fun getConstant(equality: Expression, variableIndex: Int): Expression {
        val constantIndex = 1 - variableIndex
        return equality.get(constantIndex)
    }

    private fun translateBooleanEqualityBack(variable: Expression, constant: Expression) =
            if (constant == ONE) variable else Not.make(variable)

    private fun translateNonBooleanCategoricalEqualityBack(
            variable: Expression,
            constant: Expression,
            variableIndex: Int,
            type: Categorical): Expression {

        val categoricalConstant = type.constantOfIndex(constant.intValue())
        return if (variableIndex == 0) {
            Expressions.apply(FunctorConstants.EQUALITY, variable, categoricalConstant)
        } else {
            Expressions.apply(FunctorConstants.EQUALITY, categoricalConstant, variable)
        }
    }

}