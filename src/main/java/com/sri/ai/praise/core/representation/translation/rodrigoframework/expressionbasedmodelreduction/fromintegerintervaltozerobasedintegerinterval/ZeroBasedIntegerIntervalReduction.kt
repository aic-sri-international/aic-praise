package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReduction

class ZeroBasedIntegerIntervalReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelReduction {
    override val down = FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction(expressionBasedModel)
    override val up = FromZeroBasedIntegerIntervalToIntegerIntervalUpReduction(expressionBasedModel, down.translation)
}