package org.pdfextractor.algorithm.candidate

sealed trait CandidateMetadata

case object HasPank extends CandidateMetadata
case object HasEuroSign extends CandidateMetadata

case object IsNormalLine extends CandidateMetadata
case object IsDouble extends CandidateMetadata

case object MetaPhraseType extends CandidateMetadata
