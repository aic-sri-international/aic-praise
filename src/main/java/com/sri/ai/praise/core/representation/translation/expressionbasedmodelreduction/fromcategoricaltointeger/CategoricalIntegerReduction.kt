package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromcategoricaltointeger

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReduction

class CategoricalIntegerReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelReduction {
    override val down = FromCategoricalToIntegerDownReduction(expressionBasedModel)
    override val up = FromIntegerToCategoricalUpReduction(expressionBasedModel, down.translation)
}