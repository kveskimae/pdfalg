package finder.it

import java.util.regex.Pattern

object ItalianRegexPatterns {

	val PATTERN_ITALIAN_NAME_FORBIDDEN: Pattern  = Pattern.compile("^(amministrazione|fattura)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

	// Date

	val PATTERN_ITALIAN_DATE: Pattern = Pattern.compile("([\\d]{2,2}[\\-\\s./][\\d]{2,2}[\\-\\s./]([\\d]{4,4}|[\\d]{2,2}))")

	// VATIN

	val PATTERN_ITALIAN_VATIN: Pattern = Pattern.compile("^([^\\d].*)?(\\d{11})([^\\d].*)?$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

	val PATTERN_ITALIAN_VATIN_VALUE: Pattern = Pattern.compile("(\\d{11})")

}
