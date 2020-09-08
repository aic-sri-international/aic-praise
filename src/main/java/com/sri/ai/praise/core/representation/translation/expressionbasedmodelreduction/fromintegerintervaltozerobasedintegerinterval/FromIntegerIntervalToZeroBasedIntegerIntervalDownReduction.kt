package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.*
import com.sri.ai.expresso.type.Categorical
import com.sri.ai.expresso.type.IntegerInterval
import com.sri.ai.grinder.helper.GrinderUtil
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.grinder.library.FunctorConstants.EQUALITY
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base.AbstractExpressionBasedModelDownReduction

/**
 * A class representing the down-reduction from an {@link ExpressionBasedModel}
 * possibly with non-zero-based integer interval types to another in which all such types are zero-based.
 * <p>
 * The class accomplishes its task by overriding {@link AbstractExpressionBasedModelDownReduction#processTypeName}
 * to replace each non-zero based
 * integer interval <code>l..u</code> to a shifted interval <code>0..<u - l></code>,
 * and by replacing each occurrence <code>X</code> of variables of those types by
 * <code>X + l</code>.
*/
class FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction(expressionBasedModel: ExpressionBasedModel)
    : AbstractExpressionBasedModelDownReduction(expressionBasedModel) {

    override fun processTypeName(typeName: String) = replaceIntegerIntervalByZeroBasedIntegerIntervalTypeName(typeName)

    private fun replaceIntegerIntervalByZeroBasedIntegerIntervalTypeName(typeName: String): String {
        val type = expressionBasedModel.context.getType(typeName)
        return if (type is IntegerInterval && type.nonStrictLowerBound.intValue() != 0) {
            "0..${type.cardinality().intValue() - 1}"
        } else {
            typeName
        }
    }

    override fun translateRootOf(subExpression: Expression) = translateIntegerVariables(subExpression)

    private fun translateIntegerVariables(subExpression: Expression): Expression {
        return if (expressionBasedModel.context.isVariable(subExpression)) {
            val type = expressionBasedModel.context.getTypeOfRegisteredSymbol(subExpression)
            if (type is IntegerInterval && type.nonStrictLowerBound.intValue() != 0) {
                Expressions.apply(FunctorConstants.PLUS, subExpression, type.nonStrictLowerBound)
            } else {
                subExpression
            }
        }
        else {
            subExpression
        }
    }
}