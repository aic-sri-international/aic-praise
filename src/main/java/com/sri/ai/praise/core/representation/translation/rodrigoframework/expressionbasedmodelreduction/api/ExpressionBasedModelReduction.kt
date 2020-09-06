package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel

interface ExpressionBasedModelReduction {
    val expressionBasedModel: ExpressionBasedModel
    val down: ExpressionBasedModelDownReduction
    val up: ExpressionBasedModelUpReduction
    val translation: ExpressionBasedModel
        get() = down.translation
    fun translateBack(expression: Expression) = up.translateBack(expression)
}