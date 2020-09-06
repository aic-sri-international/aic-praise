package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromcategoricaltointeger

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReducer

class CategoricalIntegerReducer: ExpressionBasedModelReducer {
    override fun invoke(expressionBasedModel: ExpressionBasedModel) = CategoricalIntegerReduction(expressionBasedModel)
}