package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromextensionalsets

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base.AbstractExpressionBasedModelUpReduction

class ExtensionalSetsUpReduction(
        upperExpressionBasedModel: ExpressionBasedModel,
        lowerExpressionBasedModel: ExpressionBasedModel
        ) : AbstractExpressionBasedModelUpReduction(upperExpressionBasedModel, lowerExpressionBasedModel) {

    override fun translateSubExpressionBack(expression: Expression?): Expression = expression!!

}