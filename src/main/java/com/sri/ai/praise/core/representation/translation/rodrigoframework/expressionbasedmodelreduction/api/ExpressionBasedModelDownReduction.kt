package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel

interface ExpressionBasedModelDownReduction {
    val expressionBasedModel: ExpressionBasedModel
    val translation: ExpressionBasedModel
}