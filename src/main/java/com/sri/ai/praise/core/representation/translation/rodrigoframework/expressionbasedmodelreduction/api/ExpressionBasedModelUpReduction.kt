package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel

interface ExpressionBasedModelUpReduction {
    val upperExpressionBasedModel: ExpressionBasedModel
    val lowerExpressionBasedModel: ExpressionBasedModel
    fun translateBack(expression: Expression): Expression
}