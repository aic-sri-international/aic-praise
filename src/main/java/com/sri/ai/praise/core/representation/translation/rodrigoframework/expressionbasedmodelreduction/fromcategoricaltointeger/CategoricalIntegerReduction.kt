package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReduction

class CategoricalIntegerReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelReduction {
    override val down = FromCategoricalToInteger(expressionBasedModel)
    override val up = FromIntegerToCategorical(expressionBasedModel, down.translation)
    override val translation = down.translation
}