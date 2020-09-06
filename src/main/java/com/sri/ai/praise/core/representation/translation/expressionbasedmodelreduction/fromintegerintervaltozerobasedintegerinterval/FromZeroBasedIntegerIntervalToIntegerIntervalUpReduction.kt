package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.makeSymbol
import com.sri.ai.expresso.type.IntegerInterval
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base.AbstractExpressionBasedModelUpReduction

/**
 * A class representing the up-reduction of expressions in
 * {@link ExpressionBasedModel} <code>lowerExpressionBasedModel</code>
 * which is down-reduced by {@link FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction}
 * from {@link ExpressionBasedModel} <code>upperExpressionBasedModel</code>,
 * to expressions in the latter.
 * <p>
 * The class accomplishes its task by translating each occurrence of a variable <code>X</code>
 * of a shifted integer interval type <code>0..<u - l></code> to <code>X - l</code>.
 * Additionally, it treats the following expressions in a special manner for greater readability:
 * <ol>
 * <li> <code>X rel c</code> where <code>rel</code> is a relational operator and <code>c</code> is a numeric constant
 * by replacing the whole expression by <code>X rel <c + l></code> for greater readability.
 * <li> <code>X rel V</code> where <code>rel</code> is a relational operator and <code>V</code> is a variable
 * of the same type by leaving it alone.
 * </ol>
 */
class FromZeroBasedIntegerIntervalToIntegerIntervalUpReduction(
        upperExpressionBasedModel: ExpressionBasedModel,
        lowerExpressionBasedModel: ExpressionBasedModel
        ) : AbstractExpressionBasedModelUpReduction(upperExpressionBasedModel, lowerExpressionBasedModel) {

    override fun translateSubExpressionBack(expression: Expression?): Expression =
            if (isRelational(expression!!)) {
                translateRelationalBack(expression)
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

    private fun translateRelationalBack(relational: Expression): Expression {
        return when {
            isVariable(relational.get(0)) -> {
                translateRelationalOnVariableBack(relational, relational.get(0), 0)
            }
            isVariable(relational.get(1)) -> {
                translateRelationalOnVariableBack(relational, relational.get(1), 1)
            }
            else -> {
                relational
            }
        }
    }

    private fun translateRelationalOnVariableBack(
            relationalOnVariable: Expression,
            variable: Expression,
            variableIndex: Int): Expression {

        val upperType = getUpperType(variable)
        return if (upperType is IntegerInterval) {
            translateIntegerRelationalBack(relationalOnVariable, variable, variableIndex, upperType)
        } else {
            relationalOnVariable
        }
    }

    private fun translateIntegerRelationalBack(
            relational: Expression,
            variable: Expression,
            variableIndex: Int,
            upperType: IntegerInterval): Expression {

        val other = getOther(relational, variableIndex)
        if (other.syntacticFormType == "Symbol") {
            return if (isVariable(other)) {
                if (getUpperType(other) == upperType) {
                    // Both arguments are variables of the same type, so there is nothing to do.
                    // We return a new instance to indicate that this is not
                    // just a replacement failure, but the actual desired final form,
                    // because returning the same function tells the replacement function to keep seeking replacements
                    // of the sub-expressions.
                    Expressions.apply(relational.functor, relational.get(0), relational.get(1))
                } else {
                    // We return the same expression so that other rewriters take care of it
                    relational
                }
            } else { // other is a constant
                translateDecomposedVariableConstantIntegerRelationalBack(
                        relational.functor, variable, other, variableIndex, upperType)
            }
        }
        else { // other is not a variable or a constant, give up
            return relational
        }
    }

    private fun getOther(relational: Expression, variableIndex: Int): Expression {
        val constantIndex = 1 - variableIndex
        return relational.get(constantIndex)
    }

    private fun translateDecomposedVariableConstantIntegerRelationalBack(
            relation: Expression,
            variable: Expression,
            constant: Expression,
            variableIndex: Int,
            type: IntegerInterval): Expression {

        val nonZeroBasedConstant = makeSymbol(constant.intValue() + type.nonStrictLowerBound.intValue())
        return if (variableIndex == 0) {
            Expressions.apply(relation, variable, nonZeroBasedConstant)
        } else {
            Expressions.apply(relation, nonZeroBasedConstant, variable)
        }
    }

    private fun getUpperType(expression: Expression) =
            GrinderUtil.getTypeOfExpression(expression, upperExpressionBasedModel.context)

    private fun isVariable(symbol: Expression) = upperExpressionBasedModel.context.isVariable(symbol)

    companion object {
        val RELATIONAL = setOf(
                FunctorConstants.EQUALITY,
                FunctorConstants.DISEQUALITY,
                FunctorConstants.GREATER_THAN,
                FunctorConstants.GREATER_THAN_OR_EQUAL_TO,
                FunctorConstants.LESS_THAN,
                FunctorConstants.LESS_THAN_OR_EQUAL_TO
        )

        private fun isRelational(expression: Expression): Boolean {
            return expression.syntacticFormType == "Function application"
                    &&
                    RELATIONAL.contains(expression.functor.toString())
        }
    }
}