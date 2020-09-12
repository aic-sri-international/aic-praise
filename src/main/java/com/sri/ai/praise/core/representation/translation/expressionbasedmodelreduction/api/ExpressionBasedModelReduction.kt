package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel

interface ExpressionBasedModelReduction {
    val expressionBasedModel: ExpressionBasedModel
    val down: ExpressionBasedModelDownReduction
    val up: ExpressionBasedModelUpReduction
    val translatedModel: ExpressionBasedModel
        get() = down.translatedModel
    fun translate(expression: Expression) = down.translate(expression)
    fun translateBack(expression: Expression) = up.translateBack(expression)
}