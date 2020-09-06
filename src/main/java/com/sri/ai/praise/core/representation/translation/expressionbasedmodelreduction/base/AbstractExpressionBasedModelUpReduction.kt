package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelUpReduction

abstract class AbstractExpressionBasedModelUpReduction(
        override val upperExpressionBasedModel: ExpressionBasedModel,
        override val lowerExpressionBasedModel: ExpressionBasedModel) : ExpressionBasedModelUpReduction {

    override fun translateBack(expression: Expression): Expression {
        return expression.replaceAllOccurrences(this::translateSubExpressionBack, lowerExpressionBasedModel.context)
    }

    protected abstract fun translateSubExpressionBack(expression: Expression?): Expression
}