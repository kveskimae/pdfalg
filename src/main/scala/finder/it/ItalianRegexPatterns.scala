package finder.it

object ItalianRegexPatterns {

	val PATTERN_ITALIAN_NAME_FORBIDDEN_AS_REGEX  = ("""^(?ims)(amministrazione|fattura)$""").r

	// Date

	val PATTERN_ITALIAN_DATE_AS_REGEX = ("""([\d]{2,2}[\-\s./][\d]{2,2}[\-\s./]([\d]{4,4}|[\d]{2,2}))""").r

	// VATIN

	val PATTERN_ITALIAN_VATIN_AS_REGEX = ("""^(?ims)([^\d].*)?(\d{11})([^\d].*)?$""").r

	val PATTERN_ITALIAN_VATIN_VALUE_AS_REGEX = ("""(\d{11})""").r

}
