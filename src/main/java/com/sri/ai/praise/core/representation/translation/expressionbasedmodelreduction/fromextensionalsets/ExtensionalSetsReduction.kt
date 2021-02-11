package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromextensionalsets

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReduction

class ExtensionalSetsReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelReduction {
    override val down = ExtensionalSetsDownReduction(expressionBasedModel)
    override val up = ExtensionalSetsUpReduction(expressionBasedModel, down.translatedModel)
}