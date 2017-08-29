package org.pdfextractor.algorithm.candidate

sealed trait CandidateFeatureType

case object HasPank extends CandidateFeatureType

case object HasEuroSign extends CandidateFeatureType

case object IsNormalLine extends CandidateFeatureType

case object IsDouble extends CandidateFeatureType

case object MetaPhraseType extends CandidateFeatureType

case object X extends CandidateFeatureType

case object Y extends CandidateFeatureType

case object Bold extends CandidateFeatureType

case object Height extends CandidateFeatureType

case object PageNo extends CandidateFeatureType
